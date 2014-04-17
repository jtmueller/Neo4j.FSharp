namespace Neo4j.FSharp

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Swensen.Unquote

// THIS CODE:
// 100,000 instances of the same three-property class, Release build:
// Real: 00:00:03.182, CPU: 00:00:03.187, GC gen0: 121, gen1: 1, gen2: 0

// PURE REFLECTION:
// 100,000 instances of the same three-property class, Release build:
// Real: 00:00:10.140, CPU: 00:00:10.140, GC gen0: 112, gen1: 2, gen2: 0

// TODO: Would we get even better performance generating F# source code and compiling it to a dynamic assembly?
// http://fsharp.github.io/FSharp.Compiler.Service/compiler.html
// Hard part would be that the dynamic assembly would need references to the types being serialized.

type PropertyExtractor<'a> private () =

    static let buildGetProperties instanceVar (props : PropertyInfo[]) =
        let propVals =
            props
            |> Seq.filter (fun p -> p.GetIndexParameters().Length = 0) // exclude indexed properties
            |> Seq.map (fun p ->
                let name = Expr.Value p.Name
                let value =
                    let propGet = Expr.PropertyGet(Expr.Var instanceVar, p)
                    Expr.Coerce(propGet, typeof<obj>)
                Expr.NewTuple [ name; value ])
            |> List.ofSeq
        Expr.NewArray(typeof<string * obj>, propVals)

    static let buildUnionTest instanceVar (t : Type) =
        let cases = FSharpType.GetUnionCases(t)
        let defaultCase =
            let name = Expr.Value "Unknown"
            let values = Expr.Value Array.empty<string * obj>
            Expr.NewTuple [ name; values ]

        cases |> Array.fold (fun preceding case ->
            let caseTest = Expr.UnionCaseTest(Expr.Var(instanceVar), case)
            let name = Expr.Value case.Name
            let values = case.GetFields() |> buildGetProperties instanceVar
            let thenExpr = Expr.NewTuple [ name; values ]
            Expr.IfThenElse(caseTest, thenExpr, preceding)) defaultCase

    static let buildExpression (t : Type) =
        let name = Expr.Value t.Name
        let instanceVar = Var.Global("x", t)

        let createOutput =
            if t = typeof<(string * obj)[]> then
                // The input is already in the right form, so we just pass it through.
                Expr.NewTuple [ name; Expr.Var(instanceVar) ]
            elif typeof<seq<string * obj>>.IsAssignableFrom t then
                // The input is the right sort of sequence, but we need to convert it to an array.
                let sequence = Expr.Coerce(Expr.Var(instanceVar), typeof<seq<string * obj>>)
                let toArray = <@ Array.ofSeq (%%sequence : seq<string * obj>) @>
                Expr.NewTuple [ name; toArray ]
            elif typeof<seq<KeyValuePair<string, obj>>>.IsAssignableFrom t then
                // This case also handles IDictionary<string, obj>
                let sequence = Expr.Coerce(Expr.Var(instanceVar), typeof<seq<KeyValuePair<string, obj>>>)
                let toArray = <@ (%%sequence : seq<KeyValuePair<string, obj>>) |> Seq.map (fun x -> x.Key, x.Value) |> Array.ofSeq @>
                Expr.NewTuple [ name; toArray ]
            elif FSharpType.IsRecord t then
                let getProps = 
                    FSharpType.GetRecordFields(t, BindingFlags.Public ||| BindingFlags.Instance) 
                    |> buildGetProperties instanceVar
                Expr.NewTuple [ name; getProps ]
            elif FSharpType.IsUnion t then
                buildUnionTest instanceVar t                
            elif FSharpType.IsTuple t then
                raise <| 
                    NotImplementedException
                       ("Tuples are not supported. Neo4j property lists must have both names and values.")
            else
                let getProps = 
                    t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) 
                    |> buildGetProperties instanceVar
                Expr.NewTuple [ name; getProps ]

        let compiled = 
            Expr.Lambda(instanceVar, createOutput)
            |> Evaluation.eval []

        compiled :?> 'a -> (string * array<string * obj>)

    /// Extracts the type name and key/value pairs for all non-indexed properties of the given object.
    static member val getProperties = buildExpression typeof<'a> with get
