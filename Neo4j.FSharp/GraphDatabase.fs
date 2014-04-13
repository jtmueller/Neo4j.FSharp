namespace Neo4j.FSharp

open System
open System.IO
open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open FSharp.Data.JsonExtensions

/// The IGraphDatabase type is immutable and threadsafe, and should be re-used
/// for the life of the application to reduce network traffic.
type IGraphDatabase =
    abstract member Uri: Uri
    abstract member Neo4jVersion: string

type internal GraphRoot = JsonProvider<"Templates/Root.json">

/// The GraphDatabase type is immutable and threadsafe, and should be re-used
/// for the life of the application to reduce network traffic.
type internal GraphDatabase (uri: Uri, root: GraphRoot.Root) =
    
    member x.Root = root
    
    interface IGraphDatabase with
        member x.Uri = uri
        member x.Neo4jVersion = root.Neo4jVersion


module GraphDb =
    
    let connect uri = async {
        let! root = GraphRoot.AsyncLoad(uri.ToString())
        return GraphDatabase(uri, root) :> IGraphDatabase
    }
        


