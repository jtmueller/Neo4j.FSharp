# Neo4j.FSharp

Neo4j.FSharp is a client library for the Neo4j graph database, intended for use from F#.
It is currently in proof-of-concept stage and is not yet usable, but already it has
a promising computation-expression-based query builder, as well as a high-speed 
compiled-expression-based serializer that transforms the public properties of nearly
any .NET type (including F# Records) to Cypher syntax.

    open Neo4j.FSharp.Cypher

    type Person =
        { Name : string
          Age : int
          Sex : char }

    type Room =
        { Name : string
          Desc : string }

    cypher {
        raw "CREATE (p1:Person { Name: 'Andres', Age: 27, Sex: 'M' })"
        createEmptyNode "n1" "Foo"
        createNode "p2" "Person" [ "Name", box "Lisa"
                                   "Age", box 32
                                   "Sex", box "F" ]
        create "p3" { Name = "Random J. Person"
                      Age = 26
                      Sex = 'M' }
        create "p4" { Name = "Mark"
                      Age = 41
                      Sex = 'M' }
        create "p5" { Name = "Gary"
                      Age = 57
                      Sex = 'M' }
        create "p6" { Name = "Malika"
                      Age = 14
                      Sex = 'F' }
        create "p7" { Name = "Outside Farmhouse"
                      Desc = "You are outside a small white farmhouse. There is a mailbox here." }
    }
    |> CypherBuilder.build
    |> printfn "%s"