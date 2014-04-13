#r @"bin\Release\Neo4j.FSharp.dll"

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