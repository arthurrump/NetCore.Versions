namespace VersionsOfDotNet

open Data
open System.Net.Http
open Thoth.Json.Net
open System
open GitHub

module Run =
    let indexUrl = sprintf "https://github.com/%s/raw/%s/release-notes/releases-index.json"
    let releasesUrl (url : string) repo hash =
        url.Replace("https://dotnetcli.blob.core.windows.net/dotnet/release-metadata",
                    sprintf "https://github.com/%s/raw/%s/release-notes" repo hash)
           .Replace("/dotnet/core/blob/master/", sprintf "/%s/raw/%s/" repo hash)

    let getAllFiles (http : HttpClient) repo hash =
        async {
            let! resp = indexUrl repo hash |> http.GetAsync |> Async.AwaitTask
            if resp.IsSuccessStatusCode then
                let! indexJson = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
                let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexEntry.Decoder))
                match Decode.fromString decoder indexJson with
                | Error ex -> return Error ex
                | Ok indexes -> 
                    let! pairs =
                        indexes
                        |> List.map (fun i -> 
                            async {
                                let url = releasesUrl i.ReleasesJson repo hash
                                let! channelResp = url |> http.GetAsync |> Async.AwaitTask
                                if channelResp.IsSuccessStatusCode then
                                    let! channelJson = channelResp.Content.ReadAsStringAsync() |> Async.AwaitTask
                                    return i, Decode.fromString Channel.Decoder channelJson 
                                else return i, Error (sprintf "Couldn't load %s: %O" url channelResp.StatusCode)
                            })
                        |> Async.Parallel
                    return Ok (pairs |> Array.toList)
            else return Error (sprintf "Couldn't load %s: %O" (indexUrl repo hash) resp.StatusCode)
        }

    let getErrors http repo hash = async {
        let maxLines num (text: string) =
            let lines = text.Split('\n')
            if num >= lines.Length then text
            else (lines.[..num - 1] |> String.concat "\n") + "\n..."

        match! getAllFiles http repo hash with
        | Error mes -> 
            return [ sprintf "Error parsing releases-index.json: %s" mes ], [ ]
        | Ok pairs ->
            let parseErrors = 
                pairs 
                |> List.choose (fun (i, rc) -> match rc with Ok _ -> None | Error mes -> Some (i, mes))
                |> List.map (fun (i, mes) -> 
                    let urlSegments = Uri(i.ReleasesJson).Segments
                    let filename = urlSegments.[urlSegments.Length - 2..] |> String.concat ""
                    sprintf "Error parsing %s: %s" filename (maxLines 7 mes))

            let! results = 
                pairs 
                |> List.choose (fun (i, rc) -> match rc with Ok r -> Some (i, r) | Error _ -> None)
                |> Checks.runAllChecks
            let consistencyErrors =
                results
                |> List.choose Checks.sprintErrors

            return parseErrors, consistencyErrors
    }

    let conclusion errors =
        match errors with
        | [], [] -> Success
        | _ -> Failure

    let outputText (parseErrors, consistencyErrors) =
        (parseErrors |> String.concat "\n") +
        (consistencyErrors |> String.concat "\n")

    let output errors =
        match errors with
        | [], [] -> None
        | p, [] -> 
            Some <|
            { Title = "Releases.json Checks failed"
              Summary = sprintf "%i Errors were found while fetching and parsing the files." p.Length
              Text = outputText errors }
        | [], c -> 
            Some <|
            { Title = "Releases.json Checks failed"
              Summary = sprintf "%i Errors were found while checking for consistency." c.Length
              Text = outputText errors }
        | p, c -> 
            Some <|
            { Title = "Releases.json Checks failed"
              Summary = sprintf "%i Schema errors and %i consistency errors were found." p.Length c.Length
              Text = outputText errors }

    let runChecks repo hash = async {
        use githubClient = new HttpClient()
        githubClient.DefaultRequestHeaders.Accept.Clear()
        githubClient.DefaultRequestHeaders.Accept.ParseAdd("application/vnd.github.antiope-preview+json")
        let postNewChecksRun = postNewChecksRun githubClient repo
        let updateChecksRun = updateChecksRun githubClient repo

        let run = 
            { Name = "Releases.json Checks"
              HeadSha = hash
              Status = InProgress
              Output = None }

        // TODO: add retry
        let! res = postNewChecksRun run
        let runId = match res with Ok id -> id | Error ex -> failwith (string ex)
    
        use fetchClient = new HttpClient()
        let! errors = getErrors fetchClient repo hash

        let run = 
            { run with 
                Status = CheckRunStatus.Completed (conclusion errors, DateTimeOffset.UtcNow)
                Output = output errors }

        let! res = updateChecksRun runId run
        match res with Ok _ -> () | Error ex -> failwith (string ex)
    }
