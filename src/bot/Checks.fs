namespace VersionsOfDotNet

open Data

open Swensen.Unquote

module Checks =
    type Result = Ok | Error of message: string
    type Data = (IndexEntry * Channel)

    let check message fn data =
        try 
            fn data
            Ok
        with
        | :? AssertionFailedException as ex -> 
            Error <| sprintf "%s, but\n%s" message ex.Message

    let checkAll message (fn: Data -> unit) =
        List.map (check message fn)

    let checks (data: Data list) =
        [ checkAll "Index latest release should equal channel latest release" <|
            fun (i, c) -> test <@ i.LatestRelease = c.LatestRelease @>
          checkAll "Index latest release date should equal channel latest release date" <|
            fun (i, c) -> test <@ i.LatestReleaseDate = c.LatestReleaseDate @>
          checkAll "Index latest runtime should equal channel latest runtime" <|
            fun (i, c) -> test <@ i.LatestRuntime = c.LatestRuntime @>
          checkAll "Index latest sdk should equal channel latest sdk" <|
            fun (i, c) -> test <@ i.LatestSdk = c.LatestSdk @>
          checkAll "Index support phase should equal channel support phase" <|
            fun (i, c) -> test <@ i.SupportPhase = c.SupportPhase @>
          checkAll "Index eol date should equal channel eol date" <|
            fun (i, c) -> test <@ i.EolDate = c.EolDate @>

          checkAll "Latest release date should be newest in list of releases" <|
            fun (_, c) -> 
                test <@ c.LatestReleaseDate = 
                         (c.Releases 
                          |> List.maxBy (fun r -> r.ReleaseDate)
                          |> fun r -> r.ReleaseDate) @>
          checkAll "Latest release should be the version of the newest release in the list" <|
            fun (_, c) -> 
                test <@ c.LatestRelease =
                         (c.Releases
                          |> List.maxBy (fun r -> r.ReleaseDate)
                          |> fun r -> r.ReleaseVersion) @>
          checkAll "Latest runtime version should be the newest runtime version in the list" <|
            fun (_, c) -> 
                test <@ c.LatestRuntime =
                         (c.Releases
                          |> List.filter (fun r -> r.Runtime.IsSome)
                          |> List.maxBy (fun r -> r.ReleaseDate)
                          |> fun r -> r.Runtime.Value.Version) @>
          checkAll "Latest sdk version should be the newest sdk version in the list" <|
            fun (_, c) -> 
                test <@ c.LatestSdk =
                         (c.Releases
                          |> List.maxBy (fun r -> r.ReleaseDate)
                          |> fun r -> r.Sdk.Version) @> ]
        |> List.collect (fun fn -> fn data)
        