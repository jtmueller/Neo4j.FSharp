namespace Neo4j.FSharp

open System
open System.Text
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

module ExpressionParser =
    open Printf
    open CypherUtils

    let private containsNoCase (target: string) (content: string) =
        content.IndexOf(target, StringComparison.InvariantCultureIgnoreCase) > -1

    let private getOperator = function
        | "op_Equality" -> " = "
        | "op_Inequality" -> " <> "
        | "op_LessThan" -> " < "
        | "op_GreaterThan" -> " > "
        | "op_LessThanOrEqual" -> " <= "
        | "op_GreaterThanOrEqual" -> " >= "
        | "op_EqualsTwiddle" -> " =~ " // Cypher regex-equals operator
        // Math operators
        | "op_Addition" -> " + "
        | "op_Subtraction" -> " - "
        | "op_Multiply" -> " * "
        | "op_Division" -> " / "
        | "op_Modulus" -> " % "
        | "op_Exponentiation" -> " ^ "
        | name ->
            failwithf "Unknown operator: %s." name
        
    let rec traverse (sb:StringBuilder) (props:PropertyBag) expr =
        match expr with
        | Value (v,t) ->
            let propName = props.NextId
            props.Params.[propName] <- v
            bprintf sb "{%s}" propName

        | Var v ->
            sb.Append(v.Name) |> ignore

        | AndAlso (le, re) ->
            bprintf sb "("
            traverse sb props le
            bprintf sb " AND "
            traverse sb props re
            bprintf sb ")"

        | OrElse (le, re) ->
            bprintf sb "("
            traverse sb props le
            bprintf sb " OR "
            traverse sb props re
            bprintf sb ")"

        | SpecificCall <@ (not) @> (None, _, arg :: []) ->
            bprintf sb "NOT ("
            traverse sb props arg
            bprintf sb ")"

        | Call(None, mi, arg :: Let(_, Value(name, nameType), Lambda(_, Call(None, mi2, _))) :: []) 
            when mi.Name = "op_PipeRight" && mi2.Name = "As" && nameType = typeof<string> ->
            // AS for RETURN: p.Age |> As "foo"
            traverse sb props arg
            bprintf sb " AS %O" (escapeIdent (string name))

        | Call(None, mi, value :: Let(var, varVal, body) :: []) when mi.Name = "op_PipeRight" ->
            // replace the variable in the expression with the actual code
            let transformed = 
                body.Substitute (fun v -> if v = var then Some varVal else None)

            match transformed with
            | Lambda(innerVar, e) ->
                // In <@ users |> Seq.filter (...) @> renames the "source" parameter
                // of Seq.filter to "users" for later use.
                e.Substitute(fun v -> if v = innerVar then Some value else None)
                |> traverse sb props
            | _ ->
                traverse sb props transformed

        | Call(None, mi, le :: Value(null, _) :: _) when mi.Name.StartsWith "op_" && containsNoCase "equality" mi.Name ->
            traverse sb props le
            let statement =
                match mi.Name with
                | "op_Equality" -> " IS NULL"
                | "op_Inequality" -> " IS NOT NULL"
                | op -> failwithf "Unknown null-comparison operator: %s." op
            sb.Append statement |> ignore

        | Call(None, mi, Value(null, _) :: re :: _) when mi.Name.StartsWith "op_" && containsNoCase "equality" mi.Name ->
            traverse sb props re
            let statement =
                match mi.Name with
                | "op_Equality" -> " IS NULL"
                | "op_Inequality" -> " IS NOT NULL"
                | op -> failwithf "Unknown null-comparison operator: %s." op
            sb.Append statement |> ignore

        | Call(None, mi, le :: re :: _) when mi.Name.StartsWith "op_" ->
            // operators
            let op = getOperator mi.Name
            traverse sb props le
            sb.Append op |> ignore
            traverse sb props re

        | NewTuple exprs ->
            let mutable first = true
            for e in exprs do
                if not first then
                    sb.Append ", " |> ignore
                else
                    first <- false
                traverse sb props e

        | Call(Some e, mi, a :: _) when mi.Name = "StartsWith" && mi.DeclaringType.Name = "String" ->
            // left(x, length("foo")) = "foo"
            let arg =
                let argb = StringBuilder()
                traverse argb props a
                argb.ToString()
            bprintf sb "LEFT("
            traverse sb props e
            bprintf sb ", LENGTH(%s)) = %s" arg arg 

        | Call(Some e, mi, a :: _) when mi.Name = "EndsWith" && mi.DeclaringType.Name = "String" ->
            // right(x, length("foo")) = "foo"
            let arg =
                let argb = StringBuilder()
                traverse argb props a
                argb.ToString()
            bprintf sb "RIGHT("
            traverse sb props e
            bprintf sb ", LENGTH(%s)) = %s" arg arg 
        
        | Call(Some e, mi, start :: tail) when mi.Name = "Substring" && mi.DeclaringType.Name = "String" ->  
            bprintf sb "SUBSTRING("
            traverse sb props e
            bprintf sb ", "
            traverse sb props start
            match tail with
            | len :: [] ->
                bprintf sb ", "
                traverse sb props len
            | _ -> ()
            bprintf sb ")"

        | Call(Some e, mi, search :: replace :: []) when mi.Name = "Replace" && mi.DeclaringType.Name = "String" ->
            bprintf sb "REPLACE("
            traverse sb props e
            bprintf sb ", "
            traverse sb props search
            bprintf sb ", "
            traverse sb props replace
            bprintf sb ")"

        | SpecificCall <@ string @> (None, _, arg :: []) ->
            bprintf sb "STR("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ float @> (None, _, arg :: []) ->
            bprintf sb "TOFLOAT("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ int @> (None, _, arg :: []) ->
            bprintf sb "TOINT("
            traverse sb props arg
            bprintf sb ")"

        | Call(Some arg, mi, []) when mi.Name = "ToString" ->
            bprintf sb "STR("
            traverse sb props arg
            bprintf sb ")"

        | Call(Some e, mi, []) when mi.Name = "Trim" && mi.DeclaringType.Name = "String" ->
            bprintf sb "TRIM("
            traverse sb props e
            bprintf sb ")"

        | Call(Some e, mi, _) when mi.Name = "TrimEnd" && mi.DeclaringType.Name = "String" ->
            bprintf sb "RTRIM("
            traverse sb props e
            bprintf sb ")"

        | Call(Some e, mi, _) when mi.Name = "TrimStart" && mi.DeclaringType.Name = "String" ->
            bprintf sb "LTRIM("
            traverse sb props e
            bprintf sb ")"

        | Call(Some e, mi, _) when mi.Name = "ToUpper" && mi.DeclaringType.Name = "String" ->
            bprintf sb "UPPER("
            traverse sb props e
            bprintf sb ")"

        | Call(Some e, mi, _) when mi.Name = "ToLower" && mi.DeclaringType.Name = "String" ->
            bprintf sb "LOWER("
            traverse sb props e
            bprintf sb ")"

        | PropertyGet (Some e, pi, []) when pi.Name = "Length" ->
            bprintf sb "LENGTH("
            traverse sb props e
            bprintf sb ")"

        | PropertyGet (Some inst, propInfo, []) ->
            traverse sb props inst
            bprintf sb ".%s" propInfo.Name

        | SpecificCall <@ abs @> (None, _, arg :: []) ->
            bprintf sb "ABS("
            traverse sb props arg
            bprintf sb ")"

        | Call(None, mi, arg :: []) when mi.Name = "Abs" && mi.DeclaringType.Name = "Math" ->
            bprintf sb "ABS("
            traverse sb props arg
            bprintf sb ")"            

        | SpecificCall <@ acos @> (None, _, arg :: [])
        | SpecificCall <@ Math.Acos @> (None, _, arg :: []) ->
            bprintf sb "ACOS("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ asin @> (None, _, arg :: [])
        | SpecificCall <@ Math.Asin @> (None, _, arg :: []) ->
            bprintf sb "ASIN("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ atan @> (None, _, arg :: [])
        | SpecificCall <@ Math.Atan @> (None, _, arg :: []) ->
            bprintf sb "ATAN("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ atan2 @> (None, _, arg1 :: arg2 :: [])
        | SpecificCall <@ Math.Atan2 @> (None, _, arg1 :: arg2 :: []) ->
            bprintf sb "ATAN2("
            traverse sb props arg1
            bprintf sb ", "
            traverse sb props arg2
            bprintf sb ")"

        | SpecificCall <@ cos @> (None, _, arg :: [])
        | SpecificCall <@ Math.Cos @> (None, _, arg :: []) ->
            bprintf sb "COS("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ Math.Exp @> (None, _, arg :: []) ->
            bprintf sb "EXP("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ floor @> (None, _, arg :: [])
        | SpecificCall <@ Math.Floor : float -> float @> (None, _, arg :: [])
        | SpecificCall <@ Math.Floor : decimal -> decimal @> (None, _, arg :: []) ->
            bprintf sb "FLOOR("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ log @> (None, _, arg :: [])
        | SpecificCall <@ Math.Log @> (None, _, arg :: []) ->
            bprintf sb "LOG("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ log10 @> (None, _, arg :: [])
        | SpecificCall <@ Math.Log10 @> (None, _, arg :: []) ->
            bprintf sb "LOG10("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ round @> (None, _, arg :: [])
        | SpecificCall <@ Math.Round : float -> float @> (None, _, arg :: [])
        | SpecificCall <@ Math.Round : decimal -> decimal @> (None, _, arg :: []) ->
            bprintf sb "ROUND("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ sign @> (None, _, arg :: []) ->
            bprintf sb "SIGN("
            traverse sb props arg
            bprintf sb ")"

        | Call (None, mi, arg :: []) when mi.Name = "Sign" && mi.DeclaringType.Name = "Math" ->
            bprintf sb "SIGN("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ sin @> (None, _, arg :: [])
        | SpecificCall <@ Math.Sin @> (None, _, arg :: []) ->
            bprintf sb "SIN("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ sqrt @> (None, _, arg :: [])
        | SpecificCall <@ Math.Sqrt @> (None, _, arg :: []) ->
            bprintf sb "SQRT("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ tan @> (None, _, arg :: [])
        | SpecificCall <@ Math.Tan @> (None, _, arg :: []) ->
            bprintf sb "TAN("
            traverse sb props arg
            bprintf sb ")"

        | Call(Some _, mi, _) when mi.Name = "NextDouble" && mi.DeclaringType.Name = "Random" ->
            bprintf sb "RAND()"

        | Call(None, mi, arg :: []) when mi.Name = "Count" ->
            bprintf sb "COUNT("
            traverse sb props arg
            bprintf sb ")"

        | Call(None, mi, arg :: []) when mi.Name = "Labels" ->
            bprintf sb "LABELS("
            traverse sb props arg
            bprintf sb ")"

        | Call(None, mi, arg :: []) when mi.Name = "Collect" ->
            bprintf sb "COLLECT("
            traverse sb props arg
            bprintf sb ")"

        | SpecificCall <@ Seq.filter @> (None, _, (Lambda(var, predicate)) :: source :: []) ->
            // [user IN users WHERE predicate]
            bprintf sb "[%s" (escapeIdent var.Name)
            bprintf sb " IN "
            traverse sb props source
            bprintf sb " WHERE "
            traverse sb props predicate
            bprintf sb "]"

        | _ ->
            failwithf "Unsupported expression: %A" expr