# Neo4j.FSharp

Neo4j.FSharp is a client library for the Neo4j graph database, intended for use from F#.
It is currently in proof-of-concept stage and is not yet usable, but already it has
a promising computation-expression-based query builder, as well as a high-speed 
quotation-based serializer that transforms the public properties of nearly
any .NET type (including F# Records) to Cypher syntax.

##Example

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
            createType "harry" "Person" [ "Name", box "Harry"
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

###Output

    CREATE (n1:Foo)
    CREATE (ron:Person { Name: 'Ron', Age: 17, Sex: 'M' })
    CREATE (harry:Person {p1})
    CREATE (arthur:Person {p2})
    CREATE (towel:Item {p3})
    CREATE (petunias:Monster {p4})
    CREATE (harry)<-[:Friend {p5}]-(ron)
    CREATE (arthur)-[:Has]->(towel)
    MATCH (p:Person)
    WHERE (p.Name =~ {p7} AND p.Age > {p8})

    Parameter Values:
    p1 = [|("Name", "Harry"); ("Age", 17); ("Sex", "M")|]
    p2 = [|("Name", "Arthur"); ("Age", 42); ("Sex", 'M')|]
    p3 = [|("Name", "Arthur's Towel"); ("Desc", "It has green and white stripes.")|]
    p4 = [|("name", "Agrajag"); ("hitPoints", 500)|]
    p5 = [|("Since", 1997)|]
    p7 = "(Br|Ch)ad"
    p8 = 17
