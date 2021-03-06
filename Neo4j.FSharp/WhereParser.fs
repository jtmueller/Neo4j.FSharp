﻿namespace Neo4j.FSharp

open System
open System.Text
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module WhereParser =
    open Printf
    open CypherUtils
    open ExpressionParser
    
    let transcribe sb props (expr:Expr<'a -> bool>) =
        match expr.Raw with
        | Lambda(_, e) ->
            newLine sb
            sb.Append "WHERE " |> ignore
            traverse sb props e
                
        | _ ->
            failwith "WHERE quotations should consist of a lambda expression that accepts the entity and returns a boolean."

