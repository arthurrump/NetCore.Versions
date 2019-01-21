namespace VersionsOfDotNet

open Data

open System
open System.Net.Http
open Thoth.Json.Net

open Microsoft.Extensions.Logging

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
            (lines |> Array.truncate num |> String.concat "\n  ") + 
            if lines.Length > num then "\n  ..." else ""

        match! getAllFiles http owner repo hash with
        | Error mes -> 
            return [ sprintf "- Error parsing releases-index.json: %s" mes ], [ ]
        | Ok pairs ->
            let parseErrors = 
                pairs 
                |> List.choose (fun (i, rc) -> match rc with Ok _ -> None | Error mes -> Some (i, mes))
                |> List.map (fun (i, mes) -> 
                    let urlSegments = Uri(i.ReleasesJson).Segments
                    let filename = urlSegments.[urlSegments.Length - 2..] |> String.concat ""
                    sprintf "- Error parsing %s: %s" filename (maxLines 7 mes))

            let! consistencyErrors = 
                pairs 
                |> List.choose (fun (i, rc) -> match rc with Ok r -> Some (i, r) | Error _ -> None)
                |> Checks.runAllChecks

            return parseErrors, consistencyErrors
    }

    let conclusion errors =
        match errors with
        | [], [] -> CheckConclusion.Success
        | _ -> CheckConclusion.Failure

    let sprintErrors title errors =
        let title = "### " + title
        match errors with
        | [] -> None
        | _ as errors -> title::errors |> String.concat "\n" |> Some

    let outputText (parseErrors, consistencyErrors) =
        [ "Schema errors", parseErrors
          "Consistency errors", consistencyErrors |> Checks.lsprintErrors ]
        |> List.choose (fun (t, e) -> sprintErrors t e)
        |> String.concat "\n\n"

    let output errors =
        let title = 
            match errors with
            | [], [] -> None
            | p, c when c |> Checks.lerrorCount > 0 -> 
                Some <| sprintf "Failed with %i schema errors and %i consistency errors" p.Length (c |> Checks.lerrorCount)
            | p, _ -> 
                Some <| sprintf "Failed with %i schema errors" p.Length
            | [], c -> 
                Some <| sprintf "Failed with %i consistency errors" (c |> Checks.lerrorCount)

        match title with 
        | Some title -> NewCheckRunOutput(title, outputText errors)
        | None -> null

    let runChecks (logger : ILogger) (installationClient : GitHubClient) owner repo hash = async {
        logger.LogInformation("Running checks for {0}/{1} {2}", owner, repo, hash)
        let newRun = NewCheckRun("Releases.json Checks", hash, Status = Nullable(StringEnum(CheckStatus.InProgress)))
        let! run = 
            try installationClient.Check.Run.Create(owner, repo, newRun) |> Async.AwaitTask
            with ex -> 
                logger.LogError(ex, "An error occured while creating a new check run for {0}/{1} {2}.", owner, repo, hash)
                raise ex
    
        use fetchClient = new HttpClient()
        let! errors = 
            try getErrors fetchClient owner repo hash
            with ex ->
                logger.LogError(ex, "An error occured while checking the releases.json files for run {0}", run.Id)
                raise ex

        let updatedRun = 
            CheckRunUpdate(
                Status = Nullable(StringEnum(CheckStatus.Completed)),
                Conclusion = Nullable(StringEnum(conclusion errors)),
                CompletedAt = Nullable(DateTimeOffset.UtcNow),
                Output = output errors)
        do! try installationClient.Check.Run.Update(owner, repo, run.Id, updatedRun) |> Async.AwaitTask |> Async.Ignore
            with ex ->
                logger.LogError(ex, "An error occured while updating check run #{0}.", run.Id)
                raise ex

        logger.LogInformation("Checks for {0}/{1} {2} finished. Result: {3}", owner, repo, hash, conclusion errors)
    }

    let runLocal owner repo hash = async {
        use fetchClient = new HttpClient()
        let! errors = getErrors fetchClient owner repo hash

        printfn "Conclusion: %O" (conclusion errors)
        match output errors with
        | null -> ()
        | ncro -> 
            printfn "Title: %s" ncro.Title
            printfn "%s" ncro.Summary
            printfn "%s" ncro.Text
    }
