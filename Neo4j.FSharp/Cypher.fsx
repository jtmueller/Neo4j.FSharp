#r @"bin\Debug\Neo4j.FSharp.dll"

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

    relate ("harry" <-|{ Friend.Since = 1997 }|- "ron")
    relate ("arthur" -|Has()|-> "towel")
}
|> CypherBuilder.build
|> printfn "%s"
