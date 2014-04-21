#r @"bin\Debug\Neo4j.FSharp.dll"

open Neo4j.FSharp.Cypher

type Person =
    { Name : string
      Age : int
      Sex : char }

type Item =
    { Name : string
      Desc : string }

type TestUnion =
    | Monster of name:string * hitPoints:int

// Marker type for a relationship that has no properties
type Has = Has

type Friend = { Since : int }

let cypherQuery, parameters =
    cypher {
        // create nodes
        createEmpty "n1" "Foo"
        raw "CREATE (ron:Person { Name: 'Ron', Age: 17, Sex: 'M' })"
        createType "harry" "Person" 
            [ "Name", box "Harry"
              "Age", box 17
              "Sex", box "M" ]
        create "arthur" { Name = "Arthur"
                          Age = 42
                          Sex = 'M' }
        create "towel" { Name = "Arthur's Towel"
                         Desc = "It has green and white stripes." }

        create "petunias" (Monster("Agrajag", 500))

        // create relationships
        relate ("harry" <-|{ Friend.Since = 1997 }|- "ron")
        relate ("arthur" -|Has|-> "towel")

        // query and filter
        matchWith "(p:Person)"
        where <@ fun (p:Person) -> p.Name =~ "(Br|Ch)ad" && p.Age > 17 @>
    }
    |> CypherBuilder.build

printfn "%s" cypherQuery

printfn "\nParameter Values:"
for kvp in parameters do
    printfn "%s = %A" kvp.Key kvp.Value

