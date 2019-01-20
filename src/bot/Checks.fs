namespace VersionsOfDotNet

open Data

open System
open System.Net
open System.Net.Http

open Swensen.Unquote

module Checks =
    type Check<'t> = 
        | Check of name: string * check: ('t -> Async<Quotations.Expr<bool>>)
        | CheckList of name: ('t -> string) * checks: Check<'t> list
        | CheckMany of name: string * checks: ('t -> Async<Quotations.Expr<bool> list>)

    type CheckResult =
        | Single of name: string option * result: Result<unit, string>
        | ResultList of name: string * results: CheckResult list

    let check name fn =
        Check (name, fun data -> async { return fn data })
            
    let checklist name checks =
        CheckList (name, checks)

    let checkmany name fn =
        CheckMany (name, fun data -> async { return fn data })

    let checkAsync name fn =
        Check (name, fn)

    let indexChannelChecks: Check<IndexEntry * Channel> =
        checklist 
            (fun (_, c) -> sprintf "Consistency between releases-index.json and %O/releases.json" c.ChannelVersion)
            [ check "Index channel version should equal channel channel version" <|
                fun (i, c) -> <@ i.ChannelVersion = c.ChannelVersion @>
              check "Index latest release should equal channel latest release" <|
                fun (i, c) -> <@ i.LatestRelease = c.LatestRelease @>
              check "Index latest release date should equal channel latest release date" <|
                fun (i, c) -> <@ i.LatestReleaseDate = c.LatestReleaseDate @>
              check "Index latest runtime should equal channel latest runtime" <|
                fun (i, c) -> <@ i.LatestRuntime = c.LatestRuntime @>
              check "Index latest sdk should equal channel latest sdk" <|
                fun (i, c) -> <@ i.LatestSdk = c.LatestSdk @>
              check "Index support phase should equal channel support phase" <|
                fun (i, c) -> <@ i.SupportPhase = c.SupportPhase @>
              check "Index eol date should equal channel eol date" <|
                fun (i, c) -> <@ i.EolDate = c.EolDate @> ]

    let channelChecks now: Check<Channel> =
        checklist (fun c -> sprintf "Consistency between channel %O information and releases" c.ChannelVersion)
            [ check "Latest release date should be newest in list of releases" <|
                fun c -> 
                    <@ c.LatestReleaseDate = 
                        (c.Releases 
                         |> List.maxBy (fun r -> r.ReleaseDate)
                         |> fun r -> r.ReleaseDate) @>
              check "Latest release should be the version of the newest release in the list" <|
                fun c -> 
                    <@ c.LatestRelease =
                        (c.Releases
                         |> List.maxBy (fun r -> r.ReleaseDate)
                         |> fun r -> r.ReleaseVersion) @>
              check "Latest runtime version should be the newest runtime version in the list" <|
                fun c -> 
                    <@ c.LatestRuntime =
                        (c.Releases
                         |> List.filter (fun r -> r.Runtime.IsSome)
                         |> List.maxBy (fun r -> r.ReleaseDate)
                         |> fun r -> r.Runtime.Value.Version) @>
              check "Latest sdk version should be the newest sdk version in the list" <|
                fun c -> 
                    <@ c.LatestSdk =
                        (c.Releases
                         |> List.maxBy (fun r -> r.ReleaseDate)
                         |> fun r -> r.Sdk.Version) @>
              checkmany "Preview channel should only have preview releases" <|
                fun c ->
                    match c.SupportPhase with
                    | "preview" ->
                        c.Releases |> List.map (fun r -> <@ Version.isPreview r.ReleaseVersion @>)
                    | _ -> [ <@ true @> ]
              check "Channel with eol-date in the past should have support-phase eol" <|
                fun c ->
                    if Option.exists (fun date -> date <= now) c.EolDate
                    then <@ c.SupportPhase = "eol" @> 
                    else <@ c.SupportPhase <> "eol" @>]

    let wellFormedUri file =
        <@ Uri.IsWellFormedUriString(file.Url, UriKind.Absolute) @>

    let releaseChecks: Check<Release> =
        checklist (fun r -> sprintf "Release %O" r.ReleaseVersion)
            [ check "Release notes link should be well formed absolute uri" <|
                fun r -> 
                    match r.ReleaseNotes with
                    | None -> <@ true @>
                    | Some url -> <@ Uri.IsWellFormedUriString(url, UriKind.Absolute) @>
              checkAsync "Request to release notes link should give status code 200 OK" <|
                fun r -> async {
                    match r.ReleaseNotes with
                    | None -> return <@ true @>
                    | Some url ->
                        if Uri.IsWellFormedUriString(url, UriKind.Absolute) then
                            use http = new HttpClient()
                            let! res = http.GetAsync(url) |> Async.AwaitTask
                            return <@ res.StatusCode = HttpStatusCode.OK @>
                        else return <@ true @>
                }
              checkmany "File urls should be well formed absolute uris" <|
                fun r -> 
                    (r.Sdk.Files |> List.map wellFormedUri) @
                    (match r.Runtime with Some rt -> rt.Files |> List.map wellFormedUri | None -> []) @
                    (match r.AspnetcoreRuntime with Some rt -> rt.Files |> List.map wellFormedUri | None -> []) @
                    (match r.Symbols with Some s -> s.Files |> List.map wellFormedUri | None -> []) ]

    let private doCheck quotation =
        try 
            test quotation
            Ok ()
        with
        | :? AssertionFailedException -> 
            let steps = quotation |> reduceFully
            let steps = 
                steps.[steps.Length - 2..]
                |> List.map decompile 
                |> String.concat " => " 
                |> String.filter (fun c -> c <> '\n')
            Error steps

    let rec run check data =
        async {
            match check with
            | Check (name, fn) ->
                let! quotation = fn data
                let res = doCheck quotation
                return Single (Some name, res)
            | CheckList (nameFn, checks) -> 
                let! res = checks |> List.map (fun check -> run check data) |> Async.Parallel
                return ResultList (nameFn data, (res |> Array.toList))
            | CheckMany (name, fn) ->
                let! quotations = fn data
                let results = quotations |> List.map doCheck |> List.map (fun res -> Single (None, res))
                return ResultList (name, results)
        }

    let runAllChecks data =
        let now = DateTime.UtcNow
        [ for (index, channel) in data do
            yield run indexChannelChecks (index, channel)
            yield run (channelChecks now) channel
            yield! channel.Releases |> List.map (fun r -> run releaseChecks r) ]
        |> Async.Parallel |> Async.map Array.toList

    let rec errorCount (result : CheckResult) =
        match result with
        | Single (_, result) ->
            match result with
            | Ok _ -> 0
            | Error msg -> 
                printfn "Error: %s" msg
                1
        | ResultList (_, results) ->
            results
            |> List.map errorCount
            |> List.sum

    let lerrorCount =
        List.map errorCount >> List.sum

    let sprintErrors (result : CheckResult) =
        let rec sprint n result =
            let indent = String.replicate n "    "
            match result with
            | Single (name, result) ->
                match result with
                | Ok _ -> None
                | Error err -> 
                    match name with
                    | Some name -> Some <| sprintf "%s* %s, but `%s`" indent name err
                    | None -> Some <| sprintf "%s* But `%s`" indent err
            | ResultList (name, results) ->
                let errors = results |> List.choose (sprint (n + 1))
                if errors |> List.isEmpty
                then None
                else Some (indent + "- " + name + ":\n" + (errors |> String.concat "\n"))
        sprint 0 result

    let lsprintErrors =
        List.choose sprintErrors
        