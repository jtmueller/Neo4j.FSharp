#r @"..\packages\FSharp.Data.2.0.5\lib\net40\FSharp.Data.dll"

open System
open System.IO
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open FSharp.Data.JsonExtensions

module Async =
    let AwaitEmptyTask (task:System.Threading.Tasks.Task) = task |> Async.AwaitIAsyncResult |> Async.Ignore

type Neo4jResult<'a> =
    | Success of 'a
    | Failure of int * string

let getJson url = async {
    let! response = Http.AsyncRequest(url, headers=[ Accept HttpContentTypes.Json; "X-Stream", "true" ], silentHttpErrors=true)
    match response.Body with
    | HttpResponseBody.Text text -> 
        if response.StatusCode = 200 then return Success(JsonValue.Parse text)
        else return Failure(response.StatusCode, text)
    | HttpResponseBody.Binary _ -> 
        return Failure(200, "Unexpected binary result.")
}

let postJson url (data:JsonValue) = async {
    let! response = data.RequestAsync(url, headers=[ Accept HttpContentTypes.Json; "X-Stream", "true" ])
    match response.Body with
    | HttpResponseBody.Text text -> 
        if response.StatusCode = 200 then return Success(JsonValue.Parse text)
        else return Failure(response.StatusCode, text)
    | HttpResponseBody.Binary _ -> 
        return Failure(200, "Unexpected binary result.")
}

let saveJsonTemplate name (template:JsonValue) = async {
    let templateFolder = DirectoryInfo(Path.Combine(__SOURCE_DIRECTORY__, "Templates"))
    if not templateFolder.Exists then templateFolder.Create()
    use fs = File.Create(Path.Combine(templateFolder.FullName, name), 0x1000, FileOptions.Asynchronous)
    use writer = new StreamWriter(fs)
    do! writer.WriteAsync(template.ToString()) |> Async.AwaitEmptyTask
}

let getAndSave name url = async {
    let! result = getJson url
    match result with
    | Success json ->
        do! saveJsonTemplate name json
    | Failure(code, message) ->
        printfn "Error retrieving '%s':\n%s" url message
}

let postAndSave name data url = async {
    let! result = postJson url data
    match result with
    | Success json ->
        do! saveJsonTemplate name json
    | Failure(code, message) ->
        printfn "Error retrieving '%s':\n%s" url message
}
    
async {
    do! getAndSave "Root.json" "http://localhost:7474/db/data"
} |> Async.RunSynchronously

let rootDoc = JsonValue.Load(Path.Combine(__SOURCE_DIRECTORY__, "Templates\\Root.json"))

let cypherQuery = 
    JsonValue.Record([| "query", JsonValue.String("MATCH (person:Person)--(movie:Movie) RETURN person, movie") |])

[ rootDoc?node.AsString() |> getAndSave "Node.json"
  //rootDoc?cypher.AsString() |> postAndSave "Cypher.json" cypherQuery
  rootDoc?batch.AsString() |> getAndSave "Batch.json"
  rootDoc?constraints.AsString() |> getAndSave "Constraints.json"
  rootDoc?indexes.AsString() |> getAndSave "Indexes.json"
  rootDoc?node_index.AsString() |> getAndSave "NodeIndex.json"
  rootDoc?node_labels.AsString() |> getAndSave "NodeLabels.json"
  rootDoc?relationship_index.AsString() |> getAndSave "RelationshipIndex.json"
  rootDoc?relationship_types.AsString() |> getAndSave "RelationshipTypes.json"
  rootDoc?transaction.AsString() |> getAndSave "Transaction.json" ]
|> Async.Parallel
|> Async.Ignore
|> Async.RunSynchronously

// If we leave the example params object empty because it should be dynamic, there's no way of setting the params.
// Have to use JsonValue directly to create types that need dynamic properties like the params object.
//type CypherQuery = JsonProvider<"Templates/CypherQuery.json">
//let foo = CypherQuery.Root("MATCH test", { JsonValue = JsonValue.Record([| "name", JsonValue.String("foo") |]) })
