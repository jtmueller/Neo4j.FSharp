namespace Neo4j.FSharp

open System
open System.Text
open System.Text.RegularExpressions
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
        | :? float | :? float32 | :? System.Numerics.BigInteger ->
            value.ToString()
        | :? string as s -> "\"" + escapeString s + "\""
        | :? char as c   -> "\"" + escapeChar c + "\""
        | :? bool -> value.ToString().ToLower()
        | :? DateTime as dt ->
            DateTimeOffset(dt).ToString("o")
        | :? DateTimeOffset as dto ->
            dto.ToString("o")
        // TODO: Does Cypher support properties that contain sub-objects? If so the
        // default case her will need to call PropertyExtractor.getProperties and serialize those. 
        | _ -> value.ToString()

    let writeProps (b:StringBuilder) (props: array<string * obj>) =
        if props.Length = 0 then () else
        bprintf b " { "
        let mapped =
            props |> Seq.map (fun (name, value) -> (escapeIdent name) + ": " + (toCypherLiteral value))
        b.Append(String.Join(", ", mapped)) |> ignore
        bprintf b " }"

module Cypher =
    open CypherUtils

    type CypherExpr =
        | Cy of buildFunction:(StringBuilder -> unit)

    type RelationKind =
        | Left of string * string
        | Right of string * string

    type Relationship<'a> =
        /// The 'props' parameter must be supplied even if it is an instance of an empty class of the form "type Foo() = class end".
        /// This object is used to determine both the relation type name and the properties.
        | R of kind:RelationKind * props: 'a

    let (-->) ln rn = Right(ln, rn)
    let (<--) ln rn = Left(ln, rn)

    (*
        TODO: I would really love to figure out how to enable something like this syntax:
        cypher {
            let! fred = create { Name="Fred"; Age=17 } // CREATE (fred:Person { Name:"Fred", Age:17 })
            let! george = create { Name="George"; Age=17 }
            relate (Brothers fred <-> george)
        }

        cypher {
            optMatch "(user:User)-[FRIENDS_WITH]-(friend:User)"
            where <@ fun (user:User) -> user.Id = 1234 @>
            andWhere <@ fun (friend:User) -> not friend.Banned @>
            return <@ fun (user, friend) -> user.As<User>(), friend.Count() @> // in this case user/friend are a predefined interface type
        }

        TODO: Also should consider using parameters to avoid Cypher-injection attacks.
    *)
    type CypherBuilderM internal () =
        let (!) = function Cy f -> f
        let newLine (b:StringBuilder) =
            if b.Length > 0 then b.AppendLine() |> ignore
        
        member __.Yield(()) = Cy(fun _ -> ())
        member __.YieldFrom f : CypherExpr = f
        member __.Combine(Cy f, Cy g) = Cy(f +> g)
        member __.Zero() = Cy(fun _ -> ())

        member __.For(xs : seq<'a>, f : 'a -> CypherExpr) =
            Cy(fun b ->
                use e = xs.GetEnumerator()
                while e.MoveNext() do
                    !(f e.Current) b
            )

        member __.While(p : unit -> bool, Cy f) =
            Cy(fun b -> while p() do f b)

        /// Adds a raw Cypher statement to the query without alteration.
        [<CustomOperation("raw", MaintainsVariableSpace=true)>]
        member __.Raw(Cy f, cypherStatement : string) =
            Cy(f +> fun b -> newLine b; b.Append cypherStatement |> ignore)

        /// Creates an empty node with a type but no properties.
        [<CustomOperation("createEmpty", MaintainsVariableSpace=true)>]
        member __.CreateEmpty(Cy f, name, nodeType) =
            Cy(f +> fun b ->
                newLine b 
                bprintf b "CREATE (%s:%s)" (escapeIdent name) (escapeIdent nodeType)
            )
               
        /// Creates a node of the given type name and public non-indexed properties of the given entity.
        [<CustomOperation("createType", MaintainsVariableSpace=true)>]
        member __.CreateType(Cy f, name, nodeType, entity:'a) =
            let _, props = PropertyExtractor.getProperties entity
            Cy(f +> fun b ->
                newLine b
                bprintf b "CREATE (%s:%s" (escapeIdent name) (escapeIdent nodeType)
                writeProps b props
                bprintf b ")"
            )

        /// Creates a node based on the type name and public non-indexed properties of the given entity.
        [<CustomOperation("create", MaintainsVariableSpace=true)>]
        member __.Create(Cy f, name, entity:'a) =
            let nodeType, props = PropertyExtractor.getProperties entity
            Cy(f +> fun b ->
                newLine b
                bprintf b "CREATE (%s:%s" (escapeIdent name) (escapeIdent nodeType)
                writeProps b props
                bprintf b ")"
            )

        /// Inserts a Cypher MATCH statement into the query.
        [<CustomOperation("getMatch", MaintainsVariableSpace=true)>]
        member __.GetMatch(Cy f, matchExpr) =
            Cy(f +> fun b ->
                newLine b
                bprintf b "MATCH %s" matchExpr
            )

        /// Inserts a Cypher OPTIONAL MATCH statement into the query.
        [<CustomOperation("optMatch", MaintainsVariableSpace=true)>]
        member __.OptMatch(Cy f, matchExpr) =
            Cy(f +> fun b ->
                newLine b
                bprintf b "OPTIONAL MATCH %s" matchExpr
            )

        /// Inserts a WHERE statement into the query, based on the given predicate.
        [<CustomOperation("where", MaintainsVariableSpace=true)>]
        member __.Where(Cy f, expr: Quotations.Expr<'a -> bool>) = 
            Cy(f +> fun b ->
                newLine b
                bprintf b "WHERE %s" "TODO: Translate the quotation."
            )

        /// Creates a new relationship between two named nodes specified earlier in the query.
        /// Does not currently support assigning a name to the created relationship. If you need that,
        /// use "raw" for now.
        [<CustomOperation("relate", MaintainsVariableSpace=true)>]
        member __.Relate<'a>(Cy f, R(kind, entity:'a)) =
            let relType, props = PropertyExtractor.getProperties entity
            Cy(f +> fun b ->
                newLine b
                match kind with
                | Left(ln, rn) ->
                    bprintf b "CREATE (%s)<-[:%s" (escapeIdent ln) (escapeIdent relType)
                    writeProps b props
                    bprintf b "]-(%s)" (escapeIdent rn)
                | Right(ln, rn) ->
                    bprintf b "CREATE (%s)-[:%s" (escapeIdent ln) (escapeIdent relType)
                    writeProps b props
                    bprintf b "]->(%s)" (escapeIdent rn)
            )

        /// Creates a new relationship between two named nodes specified earlier in the query.
        /// Does not currently support assigning a name to the created relationship or property matching
        /// on the nodes. If you need those features, use "raw" for now.
        [<CustomOperation("relateUnique", MaintainsVariableSpace=true)>]
        member __.RelateUnique<'a>(Cy f, R(kind, entity:'a)) =
            let relType, props = PropertyExtractor.getProperties entity
            Cy(f +> fun b ->
                newLine b
                match kind with
                | Left(ln, rn) ->
                    bprintf b "CREATE UNIQUE (%s)<-[:%s" (escapeIdent ln) (escapeIdent relType)
                    writeProps b props
                    bprintf b "]-(%s)" (escapeIdent rn)
                | Right(ln, rn) ->
                    bprintf b "CREATE UNIQUE (%s)-[:%s" (escapeIdent ln) (escapeIdent relType)
                    writeProps b props
                    bprintf b "]->(%s)" (escapeIdent rn)
            )

        [<CustomOperation("createUnique", MaintainsVariableSpace=true)>]
        member __.CreateUnique(Cy f, cypherStatement) =
            Cy(f +> fun b ->
                newLine b
                bprintf b "CREATE UNIQUE %s" cypherStatement
            )

        // TODO: http://docs.neo4j.org/chunked/milestone/cypher-query-lang.html

    let cypher = CypherBuilderM()

    module CypherBuilder =
        /// Builds a CypherExpr from a "cypher" computation expression into a string.
        let build (Cy f) =
            let b = StringBuilder()
            f b
            b.ToString()
