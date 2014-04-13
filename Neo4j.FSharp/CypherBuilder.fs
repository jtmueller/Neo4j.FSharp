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
        let validIdChars = Regex("""([A-Za-z][A-Za-z0-9_])*""", RegexOptions.ExplicitCapture)
        fun name ->
            if validIdChars.IsMatch(name) then name
            else "`" + name + "`"

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
        | _ -> value.ToString()

    let writeProps (b:StringBuilder) (props: seq<string * obj>) =
        bprintf b "{ "
        let mapped =
            props |> Seq.map (fun (name, value) -> (escapeIdent name) + ": " + (toCypherLiteral value))
        b.Append(String.Join(", ", mapped)) |> ignore
        bprintf b " }"

module Cypher =
    open CypherUtils

    type CypherExpr =
        | Cy of (StringBuilder -> unit) 

    type CypherBuilderM() =
        let (!) = function Cy f -> f

        member __.Yield(txt : string) = Cy(fun b -> b.Append txt |> ignore)
        member __.Yield(c : char) = Cy(fun b -> b.Append c |> ignore)
        member __.Yield(()) = Cy(fun _ -> ())
        member __.YieldFrom f : CypherExpr = f

        member __.Combine(Cy f, Cy g) = Cy(f +> g)

        member __.Delay f : CypherExpr = Cy(fun b -> !f() b)
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
            Cy(f +> fun b -> b.AppendLine cypherStatement |> ignore)

        /// Creates an empty node with a type but no properties.
        [<CustomOperation("createEmptyNode", MaintainsVariableSpace=true)>]
        member __.CreateEmptyNode(Cy f, name, nodeType) =
            Cy(f +> fun b -> 
                bprintf b "CREATE (%s:%s)\n" (escapeIdent name) (escapeIdent nodeType)
            )
               
        /// Creates a node of the given type with the given set of properties.
        [<CustomOperation("createNode", MaintainsVariableSpace=true)>]
        member __.CreateNode(Cy f, name, nodeType, props) =
            Cy(f +> fun b ->
                bprintf b "CREATE (%s:%s " (escapeIdent name) (escapeIdent nodeType)
                writeProps b props
                bprintf b ")\n"
            )

        [<CustomOperation("create", MaintainsVariableSpace=true)>]
        member __.Create(cb, name, entity:'a) =
            let nodeType, props = PropertyExtractor.getProperties entity
            __.CreateNode(cb, name, nodeType, props)

        // TODO: http://docs.neo4j.org/chunked/milestone/cypher-query-lang.html

    let cypher = CypherBuilderM()

    module CypherBuilder =
        let build (Cy f) =
            let b = StringBuilder()
            f b
            b.ToString()
