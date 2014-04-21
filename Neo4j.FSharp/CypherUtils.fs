namespace Neo4j.FSharp

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Threading
open Printf

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

module internal CypherUtils =
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