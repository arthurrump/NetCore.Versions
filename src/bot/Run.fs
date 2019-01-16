namespace VersionsOfDotNet

open Data
open System.Net.Http
open Thoth.Json.Net
open System

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
                            let! channelJson = download <| i.ReleasesJson.Replace("https://dotnetcli.blob.core.windows.net/dotnet/release-metadata", "https://github.com/dotnet/core/raw/master/release-notes")
                            return i, Decode.fromString Channel.Decoder channelJson 
                        })
                    |> Async.Parallel
                return Ok (pairs |> Array.toList)
        }

    let maxLines num (text: string) =
        let lines = text.Split('\n')
        if num >= lines.Length then text
        else (lines.[..num - 1] |> String.concat "\n") + "\n..."

    let run () = async {
        match! getAll () with
        | Error mes -> 
            printfn "Error parsing releases-index.json: %s" mes
        | Ok pairs ->
            pairs 
            |> List.choose (fun (i, rc) -> match rc with Ok _ -> None | Error mes -> Some (i, mes))
            |> List.iter (fun (i, mes) -> 
                let urlSegments = Uri(i.ReleasesJson).Segments
                let filename = urlSegments.[urlSegments.Length - 2..] |> String.concat ""
                printfn "Error parsing %s: %s\n" filename (maxLines 7 mes))

            let! results = 
                pairs 
                |> List.choose (fun (i, rc) -> match rc with Ok r -> Some (i, r) | Error _ -> None)
                |> Checks.runAllChecks
                
            results
            |> List.choose Checks.sprintErrors
            |> String.concat "\n\n"
            |> printfn "%s"
    }
