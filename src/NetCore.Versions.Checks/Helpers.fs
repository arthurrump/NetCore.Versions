namespace NetCore.Versions.Checks

[<RequireQualifiedAccess>]
module internal Async =
    let map fn asyncThing =
        async {
            let! thing = asyncThing
            return fn thing
        }

[<RequireQualifiedAccess>]
module internal Task =
    let inline Ignore task = task |> Async.AwaitTask |> Async.Ignore

[<AutoOpen>]
module internal Helpers =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then Some(s.Substring(p.Length))
        else None

    let unwrapAsyncResult res = async {
        match res with
        | Ok asyncOk -> 
            let! ok = asyncOk
            return Ok ok
        | Error e -> return Error e
    }
