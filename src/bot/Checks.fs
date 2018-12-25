namespace VersionsOfDotNet

open Data
open Swensen.Unquote
open System.Net
open System.Net.Http

module Checks =
    type Result = Ok | Error of message: string
    type Check<'t> = 't -> Result
    type CheckList<'t> =
        { Name: 't -> string
          Checks: Check<'t> list }
        member this.RunChecks data =
            { Name = this.Name data
              Errors =
                this.Checks
                |> List.map (fun fn -> fn data)
                |> List.choose (fun res -> match res with Ok -> None | Error mes -> Some mes) }
    and CheckListResult =
        { Name: string
          Errors: string list }

    let check message fn data =
        try 
            fn data
            Ok
        with
        | :? AssertionFailedException as ex -> 
            Error <| sprintf "%s, but %s" message (ex.Message.Replace("\n", " => "))
            
    let checklist name checks =
        { Name = name; Checks = checks }

    let indexChannelChecks: CheckList<IndexEntry * Channel> =
        checklist 
            (fun (i, c) -> 
                sprintf "Consistency between releases-index.json (channel %O) and releases.json (%O)" 
                    i.ChannelVersion c.ChannelVersion)
            [ check "Index channel version should equal channel channel version" <|
                fun (i, c) -> test <@ i.ChannelVersion = c.ChannelVersion @>
              check "Index latest release should equal channel latest release" <|
                fun (i, c) -> test <@ i.LatestRelease = c.LatestRelease @>
              check "Index latest release date should equal channel latest release date" <|
                fun (i, c) -> test <@ i.LatestReleaseDate = c.LatestReleaseDate @>
              check "Index latest runtime should equal channel latest runtime" <|
                fun (i, c) -> test <@ i.LatestRuntime = c.LatestRuntime @>
              check "Index latest sdk should equal channel latest sdk" <|
                fun (i, c) -> test <@ i.LatestSdk = c.LatestSdk @>
              check "Index support phase should equal channel support phase" <|
                fun (i, c) -> test <@ i.SupportPhase = c.SupportPhase @>
              check "Index eol date should equal channel eol date" <|
                fun (i, c) -> test <@ i.EolDate = c.EolDate @> ]

    let channelChecks: CheckList<Channel> =
        checklist (fun c -> sprintf "Consistency between channel %O information and releases" c.ChannelVersion)
            [ check "Latest release date should be newest in list of releases" <|
                fun c -> 
                    test <@ c.LatestReleaseDate = 
                             (c.Releases 
                              |> List.maxBy (fun r -> r.ReleaseDate)
                              |> fun r -> r.ReleaseDate) @>
              check "Latest release should be the version of the newest release in the list" <|
                fun c -> 
                    test <@ c.LatestRelease =
                             (c.Releases
                              |> List.maxBy (fun r -> r.ReleaseDate)
                              |> fun r -> r.ReleaseVersion) @>
              check "Latest runtime version should be the newest runtime version in the list" <|
                fun c -> 
                    test <@ c.LatestRuntime =
                             (c.Releases
                              |> List.filter (fun r -> r.Runtime.IsSome)
                              |> List.maxBy (fun r -> r.ReleaseDate)
                              |> fun r -> r.Runtime.Value.Version) @>
              check "Latest sdk version should be the newest sdk version in the list" <|
                fun c -> 
                    test <@ c.LatestSdk =
                             (c.Releases
                              |> List.maxBy (fun r -> r.ReleaseDate)
                              |> fun r -> r.Sdk.Version) @> ]

    let releaseChecks: CheckList<Release> =
        checklist (fun r -> sprintf "Release %O" r.ReleaseVersion)
            [ check "Request to release notes link should give status code 200 OK" <|
                fun r -> 
                    match r.ReleaseNotes with
                    | None -> ()
                    | Some url ->
                        use http = new HttpClient()
                        let res = http.GetAsync(url) |> Async.AwaitTask |> Async.RunSynchronously
                        test <@ res.StatusCode = HttpStatusCode.OK @> ]

    let runAllChecks data =
        [ for (index, channel) in data do
            yield indexChannelChecks.RunChecks (index, channel)
            yield channelChecks.RunChecks channel
            yield! channel.Releases |> List.map (fun r -> releaseChecks.RunChecks r) ]

    let printErrors results =
        results
        |> List.filter (fun res -> res.Errors.Length > 0)
        |> List.map (fun res ->
            res.Name + "\n\t" + (res.Errors |> String.concat "\n\t"))
        |> String.concat "\n\n"