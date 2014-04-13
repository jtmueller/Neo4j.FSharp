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
    raw "CREATE (n3:Person { Name: 'Andres', Age: 27, Sex: 'M' })"
    createEmptyNode "n1" "Foo"
    createNode "n2" "Person" [ "Name", box "Lisa"
                               "Age", box 32
                               "Sex", box "F" ]
    create "n4" { Name = "Joel"
                  Age = 38
                  Sex = 'M' }
    create "n5" { Name = "Mark"
                  Age = 41
                  Sex = 'M' }
    create "n6" { Name = "Gary"
                  Age = 57
                  Sex = 'M' }
    create "n7" { Name = "Malika"
                  Age = 14
                  Sex = 'F' }
    create "r1" { Name = "Outside Farmhouse"
                  Desc = "You are outside a small white farmhouse. There is a mailbox here." }
}
|> CypherBuilder.build
|> printfn "%s"