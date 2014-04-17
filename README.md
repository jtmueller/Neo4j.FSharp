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

    // Marker type for relationship that has no properties
    type Has() = class end

    type Friend = { Since : int }

    cypher {
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

        relate ("harry" <-|{ Friend.Since = 1997 }|- "ron")
        relate ("arthur" -|Has|-> "towel")
    }

###Output

    CREATE (n1:Foo)
    CREATE (ron:Person { Name: 'Ron', Age: 17, Sex: 'M' })
    CREATE (harry:Person { Name: "Harry", Age: 17, Sex: "M" })
    CREATE (arthur:Person { Name: "Arthur", Age: 42, Sex: "M" })
    CREATE (towel:Item { Name: "Arthur\'s Towel", Desc: "It has green and white stripes." })
    CREATE (petunias:Monster { name: "Agrajag", hitPoints: 500 })
    CREATE (harry)<-[:Friend { Since: 1997 }]-(ron)
    CREATE (arthur)-[:Has]->(towel)
