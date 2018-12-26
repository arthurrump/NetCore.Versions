namespace VersionsOfDotNet

open Data
open Swensen.Unquote
open System.Net
open System.Net.Http
open System

module Checks =
    type Check<'t> = 
        | Check of name: string * check: ('t -> Async<Quotations.Expr<bool>>)
        | CheckList of name: ('t -> string) * checks: Check<'t> list
        | CheckMany of name: string * checks: ('t -> Async<Quotations.Expr<bool> list>)

    type CheckResult =
        | Single of name: string * result: Result<unit, string>
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

    let channelChecks: Check<Channel> =
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
                         |> fun r -> r.Sdk.Version) @> ]

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

    module private Async =
        let map fn asyncThing =
            async {
                let! thing = asyncThing
                return fn thing
            }

    let private doCheck name quotation =
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
            Error <| sprintf "%s, but %s" name steps

    let rec run check data =
        async {
            match check with
            | Check (name, fn) ->
                let! quotation = fn data
                let res = doCheck name quotation
                return Single (name, res)
            | CheckList (nameFn, checks) -> 
                let! res = checks |> List.map (fun check -> run check data) |> Async.Parallel
                return ResultList (nameFn data, (res |> Array.toList))
            | CheckMany (name, fn) ->
                let! quotations = fn data
                let results = quotations |> List.map (doCheck name) |> List.map (fun res -> Single (name, res))
                return ResultList (name, results)
        }

    let runAllChecks data =
        [ for (index, channel) in data do
            yield run indexChannelChecks (index, channel)
            yield run channelChecks channel
            yield! channel.Releases |> List.map (fun r -> run releaseChecks r) ]
        |> Async.Parallel |> Async.map Array.toList

    let rec sprintErrors (result: CheckResult) =
        match result with
        | Single (_, result) ->
            match result with
            | Ok _ -> None
            | Error mes -> Some mes
        | ResultList (name, results) ->
            let errors = results |> List.choose sprintErrors
            if errors |> List.isEmpty
            then None
            else Some (name + ":\n\t" + (errors |> String.concat "\n\t"))
        