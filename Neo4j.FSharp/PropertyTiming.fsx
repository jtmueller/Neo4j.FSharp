#r @"bin\Release\Neo4j.FSharp.dll"
#time

open Neo4j.FSharp

type Person =
    { Name : string
      Age : int
      Sex : char }

// testing performance of property extractor
let items = 
    seq { 1 .. 100000 }
    |> Seq.map (fun i -> { Name="Joel"; Sex='M'; Age=i })
    |> Array.ofSeq

for item in items do
    PropertyExtractor.getProperties item |> ignore

for item in items do
    ReflectionPropertyExtractor.getProperties (box item) |> ignore