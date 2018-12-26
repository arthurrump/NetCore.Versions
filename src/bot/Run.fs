namespace VersionsOfDotNet

open Data
open System.Net.Http
open Thoth.Json.Net

module Run =
    let download (url: Url) =
        async {
            use http = new HttpClient()
            return! http.GetStringAsync(url) |> Async.AwaitTask 
        }

    let getAll () =
        async {
            let! indexJson = download "https://github.com/dotnet/core/raw/master/release-notes/releases-index.json"
            let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexEntry.Decoder))
            match Decode.fromString decoder indexJson with
            | Error ex -> return Error ex
            | Ok indexes -> 
                let! pairs =
                    indexes
                    |> List.map (fun i -> 
                        async {
                            let! channelJson = download <| i.ReleasesJson.Replace("blob/master", "raw/master")
                            return i, Decode.fromString Channel.Decoder channelJson 
                        })
                    |> Async.Parallel
                return Ok (pairs |> Array.toList)
        }

    [<EntryPoint>]
    let main argv =
        match getAll () |> Async.RunSynchronously with
        | Error mes -> 
            printfn "Error parsing releases-index.json: %s" mes
        | Ok pairs ->
            pairs 
            |> List.choose (fun (i, rc) -> match rc with Ok _ -> None | Error mes -> Some (i, mes))
            |> List.iter (fun (i, mes) -> printfn "Error parsing %s: %s" i.ReleasesJson mes)

            pairs 
            |> List.choose (fun (i, rc) -> match rc with Ok r -> Some (i, r) | Error _ -> None)
            |> Checks.runAllChecks
            |> Checks.sprintErrors
            |> printfn "%s"
        
        0
