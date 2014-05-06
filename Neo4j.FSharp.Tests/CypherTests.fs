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
        let output, _ =
            cypher { raw input } |> CypherBuilder.build
        test <@ output = input @>

    [<Fact>]
    let ``Cypher: Can create an empty node`` () =
        let expected = "CREATE (n1:Foo)"
        let output, _ = 
            cypher { createEmpty "n1" "Foo" } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a node from a property list`` () = 
        let expected = """CREATE (harry:Person {p1})"""
        let expParams = [| "Name", box "Harry"; "Age", box 17; "Sex", box "M" |]
        let output, ps = 
            cypher { 
                createType "harry" "Person" expParams
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>

    [<Fact>]
    let ``Cypher: Can create a node from a dictionary`` () =
        let expParams = [| "Name", box "Harry"; "Age", box 17; "Sex", box "M" |]
        let dictionary = dict expParams
        let expected = """CREATE (harry:Person {p1})"""
        let output, ps = 
            cypher { 
                createType "harry" "Person" dictionary
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>
        
    [<Fact>]
    let ``Cypher: Can create a node from a record`` () = 
        let record = { Name = "Arthur"; Age = 42; Sex = 'M' }
        let expParams = [| "Name", box record.Name; "Age", box record.Age; "Sex", box record.Sex |]
        let expected = """CREATE (arthur:Person {p1})"""
        let output, ps = 
            cypher { 
                create "arthur" record
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>

    [<Fact>]
    let ``Cypher: Can create a node from a class`` () =
        let value = Item(Name="Arthur's Towel", Desc="It has green and white stripes.")
        let expParams = [| "Name", box value.Name; "Desc", box value.Desc |]
        let expected = """CREATE (towel:Item {p1})"""
        let output, ps =
            cypher {
                create "towel" value
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>

    [<Fact>]
    let ``Cypher: Can create a node from a discriminated union`` () =
        let value = Monster("Agrajag", 500)
        let expParams = [| "name", box "Agrajag"; "hitPoints", box 500 |]
        let expected = """CREATE (petunias:Monster {p1})"""
        let output, ps =
            cypher {
                create "petunias" value
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>

    [<Fact>]
    let ``Cypher: Can create a relationship with properties`` () =
        let expParams = [| "Since", box 1997 |]
        let expected = "CREATE (harry)<-[:Friend {p1}]-(ron)"
        let output, ps =
            cypher {
                relate ("harry" <-|{ Friend.Since = 1997 }|- "ron")
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams @>

    [<Fact>]
    let ``Cypher: Can create a relationship without properties`` () =
        let expected = "CREATE (arthur)-[:Has]->(towel)"
        let output, _ =
            cypher {
                relate ("arthur" -|Has|-> "towel")
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create a set of entities`` () =
        let expected = "CREATE (_0:Person {p1})\r\nCREATE (_1:Person {p2})\r\nCREATE (_2:Person {p3})"
        let people = [
            { Name="Fred"; Age=21; Sex='M' }
            { Name="George"; Age=21; Sex='M' }
            { Name="Hermione"; Age=17; Sex='F' }
        ]
        let expParams1 = [| "Name", box "Fred"; "Age", box 21; "Sex", box 'M' |]
        let expParams2 = [| "Name", box "George"; "Age", box 21; "Sex", box 'M' |]
        let expParams3 = [| "Name", box "Hermione"; "Age", box 17; "Sex", box 'F' |]
        let output, ps =
            cypher {
                createMany people
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.ContainsKey "p1" && ps.ContainsKey "p2" && ps.ContainsKey "p3" @>
        test <@ ps.["p1"] :?> (string * obj)[] = expParams1 @>
        test <@ ps.["p2"] :?> (string * obj)[] = expParams2 @>
        test <@ ps.["p3"] :?> (string * obj)[] = expParams3 @>

    [<Fact>]
    let ``Cypher: Can compare to null`` () =
        let output, _ =
            cypher {
                // matchWith omitted
                where <@ fun (p:Person) -> p.Name = null @>
            } |> CypherBuilder.build
        test <@ output = "WHERE p.Name IS NULL" @>

        let output, _ =
            cypher {
                where <@ fun (p:Person) -> null = p.Name @>
            } |> CypherBuilder.build
        test <@ output = "WHERE p.Name IS NULL" @>

        let output, _ =
            cypher {
                where <@ fun (p:Person) -> p.Name = null && p.Age > 5 @>
            } |> CypherBuilder.build
        test <@ output = "WHERE (p.Name IS NULL AND p.Age > {p1})" @>

        let output, _ =
            cypher {
                where <@ fun (p:Person) -> p.Name <> null @>
            } |> CypherBuilder.build
        test <@ output = "WHERE p.Name IS NOT NULL" @>

        let output, _ =
            cypher {
                where <@ fun (p:Person) -> null <> p.Name @>
            } |> CypherBuilder.build
        test <@ output = "WHERE p.Name IS NOT NULL" @>

    [<Fact>]
    let ``Cypher: Where with compound expression`` () =
        let expected = "WHERE (((p.Name =~ {p1} AND p.Age > {p2}) OR LEFT(p.Name, LENGTH({p3})) = {p3}) OR SQRT(TOFLOAT(p.Age)) < {p4})"
        let output, ps =
            cypher {
                // matchWith omitted
                where <@ fun (p:Person) -> (p.Name =~ "(Br|Ch)ad" && p.Age > 17) || p.Name.StartsWith "Al" || sqrt (float p.Age) < 2.5  @>
            } |> CypherBuilder.build
        test <@ output = expected @>
        test <@ ps.["p1"] = box "(Br|Ch)ad" @>
        test <@ ps.["p2"] = box 17 @>
        test <@ ps.["p3"] = box "Al" @>
        test <@ ps.["p4"] = box 2.5 @>

    [<Fact>]
    let ``Cypher: Can return a simple property`` () =
        let expected = "RETURN p.Name"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (p:Person) -> p.Name @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can return a tuple`` () =
        let expected = "RETURN p.Name, p.Age"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (p:Person) -> p.Name, p.Age @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can return with custom name`` () =
        let expected = "RETURN p.Name AS TheName, p.Age AS HowOld"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (p:Person) -> p.Name |> As "TheName", p.Age |> As "HowOld" @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can return a count`` () =
        let expected = "RETURN COUNT(user) AS NumUsers"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (user:Person) -> Count(user) |> As "NumUsers" @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can return labels`` () =
        let expected = "RETURN user, LABELS(user)"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (user:Person) -> user, Labels(user) @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can use Collect`` () =
        let expected = "RETURN COLLECT(user.Age) AS Ages"
        let output, _ =
            cypher {
                //matchWith/where omitted
                returnWith <@ fun (user:Person) -> Collect(user.Age) |> As "Ages" @>
            } |> CypherBuilder.build
        test <@ output = expected @>

    [<Fact>]
    let ``Cypher: Can create DELETE statement`` () =
        let expected = "DELETE u, r"
        let output, _ =
            cypher {
                //matchWith/where omitted
                delete "u, r"
            } |> CypherBuilder.build
        test <@ output = expected @>
