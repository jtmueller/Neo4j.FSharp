namespace Neo4j.FSharp

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open System.Threading
open Printf

module private CypherUtils =
    let (+>) f g x = 
        f x; g x

    let escapeChar c =
        match c with 
        | '\t' -> "\\t" | '\b' -> "\\b" 
        | '\r' -> "\\r" | '\n' -> "\\n" 
        | '\f' -> "\\f" | '\'' -> "\\'"
        | '"' -> "\\\"" | '\\' -> "\\\\"
        | _ -> string c

    let escapeString =
        let escapeChars = Regex("""[\t\b\n\r\f'"\\]""")
        fun s -> escapeChars.Replace(s, fun (m:Match) -> escapeChar m.Value.[0])

    let escapeIdent =
        let validIdChars = Regex("""^[A-Za-z]{1}[A-Za-z0-9_]*$""")
        fun name ->
            if validIdChars.IsMatch(name) then name
            else "`" + name.Replace("`", "``") + "`"

    let toCypherLiteral (value:obj) =
        match value with
        | :? int | :? int64 | :? int16 | :? sbyte
        | :? uint32 | :? uint64 | :? uint16 | :? byte
        | :? float | :? float32 | :? decimal ->
            value.ToString()
        | :? System.Numerics.BigInteger ->
            "\"" + value.ToString() + "\"" // quoting this because I don't know if Neo4j can handle arbitrarily large integers
        | :? string as s -> "\"" + escapeString s + "\""
        | :? char as c   -> "\"" + escapeChar c + "\""
        | :? bool -> value.ToString().ToLower()
        | :? DateTime as dt ->
            "\"" + escapeString (DateTimeOffset(dt).ToString("o")) + "\""
        | :? DateTimeOffset as dto ->
            "\"" + escapeString (dto.ToString("o")) + "\""
        | :? (byte[]) as bytes ->
            "\"" + escapeString (Convert.ToBase64String(bytes)) + "\""
        | :? Guid as guid ->
            "\"" + escapeString (guid.ToString()) + "\""
        | _ -> 
            "\"" + escapeString (value.ToString()) + "\""

    let writeProps (b:StringBuilder) (props: array<string * obj>) =
        if props.Length = 0 then () else
        bprintf b " { "
        let mapped =
            props |> Seq.map (fun (name, value) -> (escapeIdent name) + ": " + (toCypherLiteral value))
        b.Append(String.Join(", ", mapped)) |> ignore
        bprintf b " }"

    let inline newLine (b:StringBuilder) =
        if b.Length > 0 then b.AppendLine() |> ignore

module Cypher =
    open CypherUtils

    [<NoEquality; NoComparison; Sealed>]
    type PropertyBag() =
        let mutable idx = 0
        member val Params = new Dictionary<string, obj>() :> IDictionary<_,_> with get
        member x.NextId =
            let index = Interlocked.Increment(&idx)
            "p" + (string index)
        member private x.CurIndex = idx
        member internal x.Merge(other:PropertyBag) =
            if other.CurIndex > idx then
                let mutable initial = idx
                while initial <> Interlocked.CompareExchange(&idx, other.CurIndex, initial) do
                    initial <- idx
            for kvp in other.Params do
                x.Params.[kvp.Key] <- kvp.Value
                
    [<NoEquality; NoComparison>]
    type CypherExpr =
        | Cy of buildFunction:(StringBuilder -> unit) * parameters:PropertyBag

    /// Is this a left-arrow relationship or a right-arrow relationship?
    [<NoEquality; NoComparison>]
    type RelationKind =
        | Left of leftName:string * rightName:string
        | Right of leftName:string * rightName:string

    /// A relationship.
    [<NoEquality; NoComparison>]
    type Relationship<'a> =
        | R of kind:RelationKind * relationship:'a

    /// A partial left-arrow relationship. You should pass this to the (|-) operator.
    [<NoEquality; NoComparison>]
    type LeftPartial<'a> =
        | LP of leftName:string * relationship:'a

    /// A partial right-arrow relationship. You should pass this to the (|->) operator.
    [<NoEquality; NoComparison>]
    type RightPartial<'a> =
        | RP of leftName:string * relationship:'a

    /// Right-arrow relationship, first part. Usage:
    /// "name1" -|relationType|-> "name2"
    let inline (-|) leftName relationship = 
        RP(leftName, relationship)

    /// Right-arrow relationship, second part. Usage:
    /// "name1" -|relationType|-> "name2"
    let inline (|->) (RP(leftName, relationship)) rightName = 
        R(Right(leftName, rightName), relationship)

    /// Left-arrow relationship, first part. Usage:
    /// "name1" <-|relationType|- "name2"
    let inline (<-|) (leftName) relationship = 
        LP(leftName, relationship)

    /// Left-arrow relationship, second part. Usage:
    /// "name1" <-|relationType|- "name2"
    let inline (|-) (LP(leftName, relationship)) rightName = 
        R(Left(leftName, rightName), relationship)

    [<NoEquality; NoComparison; Sealed>]
    type CypherBuilderM internal () =
            
        member __.Yield(()) = __.Zero()
        member __.Zero() = Cy((fun _ -> ()), PropertyBag())
        member __.Combine(Cy(f, pf), Cy(g, pg)) =
            pf.Merge pg
            Cy(f +> g, pf)

        /// Adds a raw Cypher statement to the query without alteration.
        [<CustomOperation("raw", MaintainsVariableSpace=true)>]
        member __.Raw(Cy(f, p), cypherStatement : string) =
            Cy((f +> fun b -> newLine b; b.Append cypherStatement |> ignore), p)

        /// Creates an empty node with a type but no properties.
        [<CustomOperation("createEmpty", MaintainsVariableSpace=true)>]
        member __.CreateEmpty(Cy(f, p), name, nodeType) =
            Cy((f +> fun b ->
                newLine b 
                bprintf b "CREATE (%s:%s)" (escapeIdent name) (escapeIdent nodeType)), p)
               
        /// Creates a node of the given type name and public non-indexed properties of the given entity.
        [<CustomOperation("createType", MaintainsVariableSpace=true)>]
        member __.CreateType(Cy(f, p), name, nodeType, entity:'a) =
            let _, props = PropertyExtractor.getProperties entity
            let paramName = p.NextId
            p.Params.[paramName] <- props
            Cy((f +> fun b ->
                newLine b
                bprintf b "CREATE (%s:%s {%s})" 
                    (escapeIdent name) (escapeIdent nodeType) paramName), p)

        /// Creates a node based on the type name and public non-indexed properties of the given entity.
        [<CustomOperation("create", MaintainsVariableSpace=true)>]
        member __.Create(Cy(f, p), name, entity:'a) =
            let nodeType, props = PropertyExtractor.getProperties entity
            let paramName = p.NextId
            p.Params.[paramName] <- props
            Cy((f +> fun b ->
                newLine b
                if props.Length = 0 then
                    bprintf b "CREATE (%s:%s)" 
                        (escapeIdent name) (escapeIdent nodeType)
                else
                    p.Params.[paramName] <- props
                    bprintf b "CREATE (%s:%s {%s})" 
                        (escapeIdent name) (escapeIdent nodeType) paramName), p)

        /// Creates many nodes based on the type name and public non-indexed properties of the given entities.
        [<CustomOperation("createMany", MaintainsVariableSpace=true)>]
        member __.CreateMany(Cy(f, p), entities:seq<'a>) =
            let entityVals = entities |> Seq.map PropertyExtractor.getProperties
            Cy((f +> fun b ->
                for nodeType, props in entityVals do
                    newLine b
                    let paramName = p.NextId
                    if props.Length = 0 then
                        bprintf b "CREATE (:%s)" (escapeIdent nodeType)
                    else
                        p.Params.[paramName] <- props
                        bprintf b "CREATE (:%s {%s})" 
                            (escapeIdent nodeType) paramName), p)

        [<CustomOperation("createUnique", MaintainsVariableSpace=true)>]
        member __.CreateUnique(Cy(f, p), cypherStatement) =
            Cy((f +> fun b ->
                newLine b
                bprintf b "CREATE UNIQUE %s" cypherStatement), p)

        [<CustomOperation("createUniqueParams", MaintainsVariableSpace=true)>]
        member __.CreateUniqueParams(Cy(f, p), cypherStatement, parameters:'a) =
            let _, props = PropertyExtractor.getProperties parameters
            for key, value in props do
                p.Params.[key] <- value
            Cy((f +> fun b ->
                newLine b
                bprintf b "CREATE UNIQUE %s" cypherStatement), p)

        /// Inserts a Cypher MATCH statement into the query.
        [<CustomOperation("matchWith", MaintainsVariableSpace=true)>]
        member __.MatchWith(Cy(f, p), matchExpr) =
            Cy((f +> fun b ->
                newLine b
                bprintf b "MATCH %s" matchExpr), p)

        /// Inserts a Cypher OPTIONAL MATCH statement into the query.
        [<CustomOperation("optMatch", MaintainsVariableSpace=true)>]
        member __.OptMatch(Cy(f, p), matchExpr) =
            Cy((f +> fun b ->
                newLine b
                bprintf b "OPTIONAL MATCH %s" matchExpr), p)

        /// Inserts a parameterized Cypher MATCH statement into the query.
        [<CustomOperation("matchWithParams", MaintainsVariableSpace=true)>]
        member __.MatchWithParams<'a>(Cy(f, p), matchExpr, parameters:'a) =
            let _, props = PropertyExtractor.getProperties parameters
            for key, value in props do
                p.Params.[key] <- value
            Cy((f +> fun b ->
                newLine b
                bprintf b "MATCH %s" matchExpr), p)

        /// Inserts a parameterized Cypher MATCH statement into the query.
        [<CustomOperation("optMatchWithParams", MaintainsVariableSpace=true)>]
        member __.OptMatchWithParams<'a>(Cy(f, p), matchExpr, parameters:'a) =
            let _, props = PropertyExtractor.getProperties parameters
            for key, value in props do
                p.Params.[key] <- value
            Cy((f +> fun b ->
                newLine b
                bprintf b "OPTIONAL MATCH %s" matchExpr), p)

        /// Inserts a WHERE statement into the query, based on the given predicate.
        [<CustomOperation("where", MaintainsVariableSpace=true)>]
        member __.Where(Cy(f, p), expr: Quotations.Expr<'a -> bool>) = 
            Cy((f +> fun b ->
                newLine b
                bprintf b "WHERE %s" "TODO: Translate the quotation."), p)

        /// Creates a new relationship between two named nodes specified earlier in the query.
        /// Does not currently support assigning a name to the created relationship. If you need that,
        /// use "raw" for now.
        [<CustomOperation("relate", MaintainsVariableSpace=true)>]
        member __.Relate<'a>(Cy(f, p), R(kind, entity:'a)) =
            let relType, props = PropertyExtractor.getProperties entity
            let paramName = p.NextId
            if props.Length > 0 then
                p.Params.[paramName] <- props
            Cy((f +> fun b ->
                newLine b
                match kind with
                | Left(leftName=ln; rightName=rn) when props.Length = 0 ->
                    bprintf b "CREATE (%s)<-[:%s]-(%s)" 
                        (escapeIdent ln) (escapeIdent relType) (escapeIdent rn)                        
                | Left(leftName=ln; rightName=rn) ->
                    bprintf b "CREATE (%s)<-[:%s {%s}]-(%s)" 
                        (escapeIdent ln) (escapeIdent relType) paramName (escapeIdent rn)
                | Right(leftName=ln; rightName=rn) when props.Length = 0 ->
                    bprintf b "CREATE (%s)-[:%s]->(%s)" 
                        (escapeIdent ln) (escapeIdent relType) (escapeIdent rn)                        
                | Right(leftName=ln; rightName=rn) ->
                    bprintf b "CREATE (%s)-[:%s {%s}]->(%s)" 
                        (escapeIdent ln) (escapeIdent relType) paramName (escapeIdent rn)), p)

        /// Creates a new relationship between two named nodes specified earlier in the query.
        /// Does not currently support assigning a name to the created relationship or property matching
        /// on the nodes. If you need those features, use "raw" for now.
        [<CustomOperation("relateUnique", MaintainsVariableSpace=true)>]
        member __.RelateUnique<'a>(Cy(f, p), R(kind, entity:'a)) =
            let relType, props = PropertyExtractor.getProperties entity
            let paramName = p.NextId
            if props.Length > 0 then
                p.Params.[paramName] <- props
            Cy((f +> fun b ->
                newLine b
                match kind with
                | Left(leftName=ln; rightName=rn) when props.Length = 0 ->
                    bprintf b "CREATE UNIQUE (%s)<-[:%s]-(%s)" 
                        (escapeIdent ln) (escapeIdent relType) (escapeIdent rn)                        
                | Left(leftName=ln; rightName=rn) ->
                    bprintf b "CREATE UNIQUE (%s)<-[:%s {%s}]-(%s)" 
                        (escapeIdent ln) (escapeIdent relType) paramName (escapeIdent rn)
                | Right(leftName=ln; rightName=rn) when props.Length = 0 ->
                    bprintf b "CREATE UNIQUE (%s)-[:%s]->(%s)" 
                        (escapeIdent ln) (escapeIdent relType) (escapeIdent rn)                        
                | Right(leftName=ln; rightName=rn) ->
                    bprintf b "CREATE UNIQUE (%s)-[:%s {%s}]->(%s)" 
                        (escapeIdent ln) (escapeIdent relType) paramName (escapeIdent rn)), p)

        // TODO: WHERE and RETURN, for starters. Also "createMany" because we can't support loops and custom operations at the same time.
        // http://docs.neo4j.org/chunked/milestone/cypher-query-lang.html

    let cypher = CypherBuilderM()

    module CypherBuilder =
        /// Builds a CypherExpr from a "cypher" computation expression into a string.
        let build (Cy(f, p)) =
            let b = StringBuilder()
            f b
            b.ToString(), p.Params
