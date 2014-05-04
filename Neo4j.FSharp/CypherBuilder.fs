namespace Neo4j.FSharp

open System
open System.Text

module Cypher =
    open Printf
    open CypherUtils
                
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

    /// This operator is a placeholder for the Cypher regex-match operator, to be used
    /// in quotation expressions in the cypher builder.
    let (=~) (value:string) (regex:string) =
        RegularExpressions.Regex.IsMatch(value, regex)

    /// Used in a returnWith statement, gives a value a new name.
    let As (label:string) (value:'a) = value

    /// Used in a returnWith statement, returns the count of a collection
    let Count (node:'a) = 0

    // TODO: support COUNT(DISTINCT foo.Bar) and COUNT(*)

    /// Used in a returnWith statement, returns the list of labels attached to a node.
    let Labels (node:'a) = Seq.empty<string>

    /// Used in a returnWith statement, collects all instances of the given property/expression into a list.
    let Collect (value:'a) = Seq.empty<'a>

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
                let mutable i = 0
                for nodeType, props in entityVals do
                    newLine b
                    if props.Length = 0 then
                        bprintf b "CREATE (_%i:%s)" i (escapeIdent nodeType)
                    else
                        let paramName = p.NextId
                        p.Params.[paramName] <- props
                        bprintf b "CREATE (_%i:%s {%s})" i (escapeIdent nodeType) paramName
                    i <- i + 1
            ), p)

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
                WhereParser.transcribe b p expr), p)

        /// Inserts a RETURN statement into the query, based on the given predicate.
        [<CustomOperation("returnWith", MaintainsVariableSpace=true)>]
        member __.ReturnWith(Cy(f, p), expr: Quotations.Expr<'a -> 'b>) =
            Cy((f +> fun b ->
                ReturnParser.transcribe b p expr), p)

        // TODO: http://docs.neo4j.org/chunked/milestone/cypher-query-lang.html

    let cypher = CypherBuilderM()

    module CypherBuilder =
        /// Builds a CypherExpr from a "cypher" computation expression into a string.
        let build (Cy(f, p)) =
            let b = StringBuilder()
            f b
            b.ToString(), p.Params
