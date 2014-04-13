namespace Neo4j.FSharp

open System
open System.Collections.Concurrent
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Swensen.Unquote

// 100,000 instances of the same three-property class, Release build:
// Real: 00:00:10.140, CPU: 00:00:10.140, GC gen0: 112, gen1: 2, gen2: 0

type ReflectionPropertyExtractor private () =

    static let getPropVals instanceVar (props : PropertyInfo[]) =
        props
        |> Seq.filter (fun p -> p.GetIndexParameters().Length = 0) // exclude indexed properties
        |> Seq.map (fun p ->
            let name = p.Name
            let value = p.GetValue(instanceVar)
            name, value)
        |> Array.ofSeq

    /// Extracts the type name and key/value pairs for all non-indexed properties of the given object.
    static member getProperties (item:obj) =
        let t = item.GetType()
        let name = t.Name 
        if t = typeof<(string * obj)[]> then
            // The input is already in the right form, so we just pass it through.
            name, item :?> (string * obj)[]
        elif typeof<seq<string * obj>>.IsAssignableFrom t then
            // The input is the right sort of sequence, but we need to convert it to an array.
            name, Array.ofSeq (item :?> seq<string * obj>)
        elif FSharpType.IsRecord t then
            let props = 
                FSharpType.GetRecordFields(t, BindingFlags.Public ||| BindingFlags.Instance) 
                |> getPropVals item
            name, props
        elif FSharpType.IsTuple t then
            raise <| 
                NotImplementedException
                    ("Tuples are not supported. Neo4j property lists must have both names and values.")
        elif FSharpType.IsUnion t then
            raise <| 
                NotImplementedException
                    ("Discriminated Unions are not supported. Neo4j property lists must have both names and values.")
        else
            let props = 
                t.GetProperties(BindingFlags.Public ||| BindingFlags.Instance) 
                |> getPropVals item
            name, props
