namespace VersionsOfDotNet

open System
open VersionsOfDotNet // To get our Version type, instead of the one in System
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

module Data =
    // Helpers
    module private Decode =
        let version path value =
            Decode.string path value
            |> (fun r -> match r with
                         | Ok s ->
                             match Version.parse s with
                             | Some v -> Ok v
                             | None -> (path, Decode.BadPrimitive("a version", value)) |> Result.Error
                         | Result.Error v -> Result.Error v)

    let private getOptionalDate (get: Decode.IGetters) jsonName =
        get.Required.Field jsonName Decode.string
        |> fun s -> if String.IsNullOrWhiteSpace s then (false, DateTime()) else DateTime.TryParse(s)
        |> fun (s, d) -> if s then Some d else None

    type Url = string
    type DisplayVersion = string

    // releases-index.json
    type IndexEntry =
        { ChannelVersion: Version
          LatestRelease: Version
          LatestReleaseDate: DateTime
          LatestRuntime: Version
          LatestSdk: Version
          Product: string
          SupportPhase: string
          EolDate: DateTime option
          ReleasesJson: Url }

        static member Decoder : Decode.Decoder<IndexEntry> = 
            Decode.object
                (fun get -> 
                    { ChannelVersion = get.Required.Field "channel-version" Decode.version
                      LatestRelease = get.Required.Field "latest-release" Decode.version
                      LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                      LatestRuntime = get.Required.Field "latest-runtime" Decode.version
                      LatestSdk = get.Required.Field "latest-sdk" Decode.version
                      Product = get.Required.Field "product" Decode.string
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = getOptionalDate get "eol-date"
                      ReleasesJson = get.Required.Field "releases.json" Decode.string })

    // releases.json
    type Channel =
        { ChannelVersion: Version
          LatestRelease: Version
          LatestReleaseDate: DateTime
          LatestRuntime: Version
          LatestSdk: Version
          SupportPhase: string
          EolDate: DateTime option
          LifecyclePolicy: Url
          Releases: Release list }

        static member Decoder : Decode.Decoder<Channel> =
            Decode.object
                (fun get ->
                    { ChannelVersion = get.Required.Field "channel-version" Decode.version
                      LatestRelease = get.Required.Field "latest-release" Decode.version
                      LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                      LatestRuntime = get.Required.Field "latest-runtime" Decode.version
                      LatestSdk = get.Required.Field "latest-sdk" Decode.version
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = getOptionalDate get "eol-date"
                      LifecyclePolicy = get.Required.Field "lifecycle-policy" Decode.string
                      Releases = get.Required.Field "releases" (Decode.list Release.Decoder) })
    
    and Release = 
        { ReleaseDate: DateTime
          ReleaseVersion: Version
          Security: bool
          CveList: string list option // (?) This field is null everywhere...
          ReleaseNotes: Url option
          Runtime: Runtime option
          Sdk: Sdk
          AspnetcoreRuntime: AspnetcoreRuntime option
          Symbols: Symbols option }

        static member Decoder : Decode.Decoder<Release> =
            Decode.object
                (fun get -> 
                    { ReleaseDate = get.Required.Field "release-date" Decode.datetime
                      ReleaseVersion = get.Required.Field "release-version" Decode.version
                      Security = get.Required.Field "security" Decode.bool
                      CveList = get.Optional.Field "cve-list" (Decode.list Decode.string)
                      ReleaseNotes = get.Optional.Field "release-notes" Decode.string
                      Runtime = get.Optional.Field "runtime" Runtime.Decoder
                      Sdk = get.Required.Field "sdk" Sdk.Decoder
                      AspnetcoreRuntime = get.Optional.Field "aspnetcore-runtime" AspnetcoreRuntime.Decoder
                      Symbols = get.Optional.Field "symbols" Symbols.Decoder })

    and Runtime =
        { Version: Version
          VersionDisplay: DisplayVersion option
          VsVersion: DisplayVersion option
          Files: File list }

        static member Decoder : Decode.Decoder<Runtime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VsVersion = get.Optional.Field "vs-version" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and Sdk = 
        { Version: Version
          VersionDisplay: DisplayVersion option
          RuntimeVersion: Version option
          VsVersion: DisplayVersion option
          CsharpVersion: DisplayVersion option
          FsharpVersion: DisplayVersion option
          VbVersion: DisplayVersion option
          Files: File list }

        static member Decoder : Decode.Decoder<Sdk> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      RuntimeVersion = get.Optional.Field "runtime-version" Decode.version
                      VsVersion = get.Optional.Field "vs-version" Decode.string
                      CsharpVersion = get.Optional.Field "csharp-version" Decode.string
                      FsharpVersion = get.Optional.Field "fsharp-version" Decode.string
                      VbVersion = get.Optional.Field "vb-version" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and AspnetcoreRuntime =
        { Version: Version
          VersionDisplay: DisplayVersion option
          VersionAspnetcoremodule: Version list option
          Files: File list }

        static member Decoder : Decode.Decoder<AspnetcoreRuntime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VersionAspnetcoremodule = 
                        get.Optional.Field "version-aspnetcoremodule" (Decode.list Decode.version)
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and Symbols =
        { Version: Version
          Files: File list }

        static member Decoder : Decode.Decoder<Symbols> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and File =
        { Name: string
          Rid: string option
          Url: Url
          Hash: string }

        static member Decoder : Decode.Decoder<File> =
            Decode.object
                (fun get ->
                    { Name = get.Required.Field "name" Decode.string
                      Rid = get.Optional.Field "rid" Decode.string
                      Url = get.Required.Field  "url" Decode.string
                      Hash = get.Required.Field "hash" Decode.string })
