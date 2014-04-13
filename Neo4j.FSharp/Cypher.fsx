#r @"bin\Debug\Neo4j.FSharp.dll"

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

type Friend = { Since : int }

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

    relate (R("p2" --> "p3", { Since = 1982 }))
    relate (R("p3" --> "r1", StandingIn()))
}
|> CypherBuilder.build
|> printfn "%s"
