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

    type Room =
        { Name : string
          Desc : string }

    // Marker type for relationship that has no properties
    type StandingIn() = class end

    type Friend =
        { Since : int }

    cypher {
        raw "CREATE (p1:Person { Name: 'Andres', Age: 27, Sex: 'M' })"
        createEmpty "n1" "Foo"
        createType "p2" "Person" [ "Name", box "Lisa"
                                   "Age", box 32
                                   "Sex", box "F" ]
        create "p3" { Name = "Random J. Person"
                      Age = 26
                      Sex = 'M' }
        create "r1" { Name = "Outside Farmhouse"
                      Desc = "You are outside a small white farmhouse. There is a mailbox here." }

        relate (R("p2" --> "p3", { Friend.Since = 1982 }))
        relate (R("p3" --> "r1", StandingIn()))
    }
    |> CypherBuilder.build
    |> printfn "%s"

###Output

    CREATE (p1:Person { Name: 'Andres', Age: 27, Sex: 'M' })
    CREATE (n1:Foo)
    CREATE (p2:Person { Name: "Lisa", Age: 32, Sex: "F" })
    CREATE (p3:Person { Name: "Random J. Person", Age: 26, Sex: "M" })
    CREATE (r1:Room { Name: "Outside Farmhouse", Desc: "You are outside a small white farmhouse. There is a mailbox here." })
    CREATE (p2)-[:Friend { Since: 1982 }]->(p3)
    CREATE (p3)-[:StandingIn]->(r1)
