namespace Neo4j.FSharp.Tests

open System
open Xunit
open Swensen.Unquote
open Neo4j.FSharp
open Neo4j.FSharp.Cypher

module CypherTests =

    type Person =
        { Name : string
          Age : int
          Sex : char }

    type Friend = { Since : int }

    type Item() =
        member val Name = "" with get, set
        member val Desc = "" with get, set

    type Has = Has

    type SupportedPrimitives() =
        member val String = "hello" with get
        member val SByte = 42y with get
        member val Byte = 42uy with get
        member val Int16 = 42s with get
        member val UInt16 = 42us with get
        member val Int = 42 with get
        member val UInt = 42u with get
        member val Int64 = 42L with get
        member val UInt64 = 42UL with get
        member val BigInt = 42I with get
        member val Float32 = 89.3f with get
        member val Float = 89.3 with get
        member val Decimal = 89.3m with get
        member val Char = 'c' with get
        member val Boolean = true with get
        member val DateTime = DateTime(2063, 4, 5) with get
        member val DateTimeOffset = DateTimeOffset(DateTime(2063, 4, 5), TimeSpan.FromHours(-6.0)) with get
        member val Guid = Guid("e8ce994d-74f2-492e-8e71-dbce8581622d") with get
        member val Bytes = [| 104uy; 101uy; 108uy; 108uy; 111uy |] with get

    type TestUnion =
        | Monster of name:string * hitPoints:int

    [<Fact>]
    let ``Cypher: Can echo a raw statement`` () =
        let input = "CREATE (ron:Person { Name: 'Ron', Age: 17, Sex: 'M' })"
        let output =
            cypher { raw input } |> CypherBuilder.build
        test <@ output = input @>

    [<Fact>]
    let ``Cypher: Can create an empty node`` () =
        let expected = "CREATE (n1:Foo)"
        let output = 
            cypher { createEmpty "n1" "Foo" } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a node from a property list`` () = 
        let expected = """CREATE (harry:Person { Name: "Harry", Age: 17, Sex: "M" })"""
        let output = 
            cypher { 
                createType "harry" "Person" [ "Name", box "Harry"
                                              "Age", box 17
                                              "Sex", box "M" ]
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a node from a dictionary`` () = 
        let dictionary = dict [ "Name", box "Harry"; "Age", box 17; "Sex", box "M" ]
        let expected = """CREATE (harry:Person { Name: "Harry", Age: 17, Sex: "M" })"""
        let output = 
            cypher { 
                createType "harry" "Person" dictionary
            } |> CypherBuilder.build
        test <@ output = expected @>
        
    [<Fact>]
    let ``Cypher: Can create a node from a record`` () = 
        let expected = """CREATE (arthur:Person { Name: "Arthur", Age: 42, Sex: "M" })"""
        let output = 
            cypher { 
                create "arthur" { Name = "Arthur"
                                  Age = 42
                                  Sex = 'M' }
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a node from a class`` () =
        let expected = """CREATE (towel:Item { Name: "Arthur\'s Towel", Desc: "It has green and white stripes." })"""
        let output =
            cypher {
                create "towel" 
                    (Item(Name="Arthur's Towel", 
                          Desc="It has green and white stripes."))
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a node from a discriminated union`` () =
        let expected = """CREATE (petunias:Monster { name: "Agrajag", hitPoints: 500 })"""
        let output =
            cypher {
                create "petunias" (Monster("Agrajag", 500))
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a relationship with properties`` () =
        let expected = "CREATE (harry)<-[:Friend { Since: 1997 }]-(ron)"
        let output =
            cypher {
                relate ("harry" <-|{ Friend.Since = 1997 }|- "ron")
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a relationship without properties`` () =
        let expected = "CREATE (arthur)-[:Has]->(towel)"
        let output =
            cypher {
                relate ("arthur" -|Has|-> "towel")
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can encode supported primitive types`` () =
        let expected = """CREATE (types:SupportedPrimitives { String: "hello", SByte: 42, Byte: 42, Int16: 42, UInt16: 42, Int: 42, UInt: 42, Int64: 42, UInt64: 42, BigInt: "42", Float32: 89.3, Float: 89.3, Decimal: 89.3, Char: "c", Boolean: true, DateTime: "2063-04-05T00:00:00.0000000-05:00", DateTimeOffset: "2063-04-05T00:00:00.0000000-06:00", Guid: "e8ce994d-74f2-492e-8e71-dbce8581622d", Bytes: "aGVsbG8=" })"""
        let output =
            cypher {
                create "types" (SupportedPrimitives())
            } |> CypherBuilder.build
        test <@ output = expected @>

//    [<Fact>]
//    let ``Cypher: Can create a set of entities`` () =
//        let expected = ""
//        let people = [
//            { Name="Fred"; Age=21; Sex='M' }
//            { Name="George"; Age=21; Sex='M' }
//            { Name="Hermione"; Age=17; Sex='F' }
//        ]
//        let output =
//            cypher {
//                for person in people do
//                    create "" person
//            } |> CypherBuilder.build
//        test <@ output = expected @>
