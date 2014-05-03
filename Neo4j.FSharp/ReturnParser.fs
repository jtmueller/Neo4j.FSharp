namespace Neo4j.FSharp

open System
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module ReturnParser =
    open Printf
    open CypherUtils
    open ExpressionParser
    
    let transcribe sb props (expr:Expr<'a -> 'b>) =
        match expr.Raw with
        | Lambda(_, e) ->
            newLine sb
            sb.Append "RETURN " |> ignore
            traverse sb props e
                
        | _ ->
            failwith "RETURN quotations should consist of a lambda expression that accepts the entities and returns the desired output."

