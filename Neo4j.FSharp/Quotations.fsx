#r @"bin\Debug\Neo4j.FSharp.dll"

open System
open Neo4j.FSharp
open Neo4j.FSharp.Cypher

type Person = 
    { Name : string
      Age : int
      Sex : char }

type Point = 
    { X : float
      Y : float }

let sb = Text.StringBuilder()
let props = PropertyBag()

<@ fun (p:Person) -> p.Age < 30 @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Sex = 'F' @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name =~ "(Ch|Br)ad" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Age * 2 < 80 @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> 3.0 ** 4.0 > 80.0 @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> (p.Name = "Joel" && p.Age > 5) || p.Sex = 'F' @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.StartsWith("J") @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.Length = 37 @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.Substring(0, 2) = "Al" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> not (p.Name.Substring(0, 2) = "Al") @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.Replace("Br", "Ch") = "Chad" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.EndsWith "ad" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.Trim() = "Joel" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.TrimEnd() = "Joel" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.TrimStart() = "Joel" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.ToUpper() = "JOEL" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Name.ToLower() = "JOEL" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> string p.Age = "27" @>
|> WhereParser.transcribe sb props

<@ fun (p:Person) -> p.Age.ToString() = "27" @>
|> WhereParser.transcribe sb props

<@ fun (p:Point) -> Math.Abs(p.X) < 4.0 @>
|> WhereParser.transcribe sb props

<@ fun (p:Point) -> abs p.X < 4.0 @>
|> WhereParser.transcribe sb props

<@ fun (p:Point) -> atan2 p.X p.Y > 3.0 @>
|> WhereParser.transcribe sb props

<@ fun (p:Point) -> Math.Atan2(p.X, p.Y) > 3.0 @>
|> WhereParser.transcribe sb props

<@ fun (p:Point) -> Math.Floor p.X > 3.0 @>
|> WhereParser.transcribe sb props

let rand = Random()
<@ fun (p:Point) -> p.X = rand.NextDouble() @>
|> WhereParser.transcribe sb props

printfn "%O" sb

printfn "\nParameter Values:"
for kvp in props.Params do
    printfn "%s = %A" kvp.Key kvp.Value