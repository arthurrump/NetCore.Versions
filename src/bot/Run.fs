namespace VersionsOfDotNet

open Data

open System
open System.Net.Http
open Thoth.Json.Net

open Octokit

module Run =
    let indexUrl = sprintf "https://github.com/%s/%s/raw/%s/release-notes/releases-index.json"
    let releasesUrl (url : string) owner repo hash =
        url.Replace("https://dotnetcli.blob.core.windows.net/dotnet/release-metadata",
                    sprintf "https://github.com/%s/%s/raw/%s/release-notes" owner repo hash)
           .Replace("/dotnet/core/blob/master/", sprintf "/%s/%s/raw/%s/" owner repo hash)

    let getReleasesJson (http : HttpClient) owner repo hash releasesJson = async {
        let url = releasesUrl releasesJson owner repo hash
        let! channelResp = url |> http.GetAsync |> Async.AwaitTask
        if channelResp.IsSuccessStatusCode then
            let! channelJson = channelResp.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Decode.fromString Channel.Decoder channelJson 
        else return Error (sprintf "Couldn't load %s: %O" url channelResp.StatusCode)
    }

    let getAllFiles (http : HttpClient) owner repo hash = async {
        let! resp = indexUrl owner repo hash |> http.GetAsync |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! indexJson = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let decoder = Decode.object (fun get -> get.Required.Field "releases-index" (Decode.list IndexEntry.Decoder))
            match Decode.fromString decoder indexJson with
            | Error ex -> return Error ex
            | Ok indexes -> 
                let! pairs =
                    indexes
                    |> List.map (fun i -> getReleasesJson http owner repo hash i.ReleasesJson |> Async.map (fun r -> i, r))
                    |> Async.Parallel
                return Ok (pairs |> Array.toList)
        else return Error (sprintf "Couldn't load %s: %O" (indexUrl owner repo hash) resp.StatusCode)
    }

    let getErrors http owner repo hash = async {
        let maxLines num (text: string) =
            let lines = text.Split('\n')
            if num >= lines.Length then text
            else (lines.[..num - 1] |> String.concat "\n") + "\n..."

        match! getAllFiles http owner repo hash with
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
        | [], [] -> CheckConclusion.Success
        | _ -> CheckConclusion.Failure

    let outputText (parseErrors, consistencyErrors) =
        (parseErrors |> String.concat "\n") +
        (consistencyErrors |> String.concat "\n")

    let output errors =
        let summary = 
            match errors with
            | [], [] -> None
            | p, [] -> 
                Some <| sprintf "%i Errors were found while fetching and parsing the files." p.Length
            | [], c -> 
                Some <| sprintf "%i Errors were found while checking for consistency." c.Length
            | p, c -> 
                Some <| sprintf "%i Schema errors and %i consistency errors were found." p.Length c.Length

        match summary with 
        | Some s -> NewCheckRunOutput("Releases.json Checks failed", s, Text = outputText errors)
        | None -> null

    let runChecks (installationClient : GitHubClient) owner repo hash = async {
        let newRun = NewCheckRun("Releases.json Checks", hash, Status = Nullable(StringEnum(CheckStatus.InProgress)))
        let! run = installationClient.Check.Run.Create(owner, repo, newRun) |> Async.AwaitTask
    
        use fetchClient = new HttpClient()
        let! errors = getErrors fetchClient owner repo hash

        let updatedRun = 
            CheckRunUpdate(
                Status = Nullable(StringEnum(CheckStatus.Completed)),
                Conclusion = Nullable(StringEnum(conclusion errors)),
                CompletedAt = Nullable(DateTimeOffset.UtcNow),
                Output = output errors)
        do! installationClient.Check.Run.Update(owner, repo, run.Id, updatedRun) |> Async.AwaitTask |> Async.Ignore
    }