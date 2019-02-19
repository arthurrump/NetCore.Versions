namespace NetCore.Versions.Checks

open NetCore.Versions.Data

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

    let indexDecoder = Decode.field "releases-index" (Decode.list IndexEntry.Decoder)

    let tryGet (http : HttpClient) (url : string) = async {
        let! resp = url |> http.GetAsync |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! body = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Ok body
        else return Error (sprintf "Couldn't load %s: %O" url resp.StatusCode)
    }

    let getReleasesJson tryGet owner repo hash releasesJson = async {
        let! resp = tryGet (releasesUrl releasesJson owner repo hash)
        return resp |> Result.bind (fun json -> Decode.fromString Channel.Decoder json)
    }

    let getAllFiles tryGet owner repo hash = async {
        let! resp = tryGet (indexUrl owner repo hash)
        let! pairs =
            resp
            |> Result.bind (Decode.fromString indexDecoder)
            |> Result.map 
                (List.map (fun i -> getReleasesJson tryGet owner repo hash i.ReleasesJson |> Async.map (fun r -> i, r))
                 >> Async.Parallel)
            |> unwrapAsyncResult
        return pairs |> Result.map Array.toList
    }

    let getResults http owner repo hash = async {
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

    let conclusion results =
        match fst results, snd results |> Checks.lerrorCount with
        | [], 0 -> CheckConclusion.Success
        | _ -> CheckConclusion.Failure

    let sprintErrors title errors =
        let title = "### " + title
        match errors with
        | [] -> None
        | _ as errors -> title::errors |> String.concat "\n" |> Some

    let outputText (parseErrors, consistencyResults) =
        [ "Schema errors", parseErrors
          "Consistency errors", consistencyResults |> Checks.lsprintErrors ]
        |> List.choose (fun (t, e) -> sprintErrors t e)
        |> String.concat "\n\n"

    let output results =
        let title = 
            match fst results |> List.length, snd results |> Checks.lerrorCount with
            | 0, 0 -> None
            | s, 0 -> 
                Some <| sprintf "Failed with %i schema errors" s
            | 0, c -> 
                Some <| sprintf "Failed with %i consistency errors" c
            | s, c -> 
                Some <| sprintf "Failed with %i schema errors and %i consistency errors" s c

        match title with 
        | Some title -> NewCheckRunOutput(title, outputText results)
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
        let tryGet = tryGet fetchClient

        let! results = 
            try getResults tryGet owner repo hash
            with ex ->
                logger.LogError(ex, "An error occured while checking the releases.json files for run {0}", run.Id)
                raise ex

        let updatedRun = 
            CheckRunUpdate(
                Status = Nullable(StringEnum(CheckStatus.Completed)),
                Conclusion = Nullable(StringEnum(conclusion results)),
                CompletedAt = Nullable(DateTimeOffset.UtcNow),
                Output = output results)
        do! try installationClient.Check.Run.Update(owner, repo, run.Id, updatedRun) |> Task.Ignore
            with ex ->
                logger.LogError(ex, "An error occured while updating check run #{0}.", run.Id)
                raise ex

        logger.LogInformation("Checks for {0}/{1} {2} finished. Result: {3}", owner, repo, hash, conclusion results)
    }

    let runLocal owner repo hash = async {
        use fetchClient = new HttpClient()
        let tryGet = tryGet fetchClient

        let! results = getResults tryGet owner repo hash

        printfn "Conclusion: %O" (conclusion results)
        match output results with
        | null -> ()
        | ncro -> 
            printfn "Title: %s" ncro.Title
            printfn "%s" ncro.Summary
            printfn "%s" ncro.Text
    }
