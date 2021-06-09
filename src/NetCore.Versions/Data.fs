namespace NetCore.Versions

open System
open NetCore.Versions // To get our Version type, instead of the one in System
open Thoth.Json.Net
open Newtonsoft.Json.Linq

module Data =
    // Helpers
    module private Result =
        let rec allOk = 
            function
            | [] -> Ok []
            | Error x::_ -> Error x
            | Ok x::rest -> Result.map (fun r -> x::r) (allOk rest)

    module Decode =
        let version path value =
            match Decode.string path value with
            | Ok s ->
                match Version.parse s with
                | Some v -> Ok v
                | None -> (path, BadPrimitive("a version", value)) |> Result.Error
            | Error v -> Error v

        let separatedString (sep : char) decoder path value =
            match Decode.string path value with
            | Ok str ->
                str.Split(sep) 
                |> Array.toList
                |> List.map (fun s -> decoder path (JValue(s) :> JToken))
                |> Result.allOk
            | Error v -> Error v

        let emptyStringAsNone decoder path value =
            match Decode.string path value with
            | Ok "" -> Ok None
            | Ok _ -> decoder path value |> Result.map Some
            | Error v -> Error v

    type Url = string
    type DisplayVersion = string

    // releases-index.json
    type IndexEntry =
        { ChannelVersion: Version
          LatestRelease: Version
          LatestReleaseDate: DateTime
          Security: bool
          LatestRuntime: Version
          LatestSdk: Version
          Product: string
          SupportPhase: string
          EolDate: DateTime option
          ReleasesJson: Url }

        static member Decoder : Decoder<IndexEntry> = 
            Decode.object
                (fun get -> 
                    { ChannelVersion = get.Required.Field "channel-version" Decode.version
                      LatestRelease = get.Required.Field "latest-release" Decode.version
                      LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                      Security = get.Required.Field "security" Decode.bool
                      LatestRuntime = get.Required.Field "latest-runtime" Decode.version
                      LatestSdk = get.Required.Field "latest-sdk" Decode.version
                      Product = get.Required.Field "product" Decode.string
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = get.Optional.Field "eol-date" Decode.datetime
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

        static member Decoder : Decoder<Channel> =
            Decode.object
                (fun get ->
                    { ChannelVersion = get.Required.Field "channel-version" Decode.version
                      LatestRelease = get.Required.Field "latest-release" Decode.version
                      LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                      LatestRuntime = get.Required.Field "latest-runtime" Decode.version
                      LatestSdk = get.Required.Field "latest-sdk" Decode.version
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = get.Optional.Field "eol-date" Decode.datetime
                      LifecyclePolicy = get.Required.Field "lifecycle-policy" Decode.string
                      Releases = get.Required.Field "releases" (Decode.list Release.Decoder) })
    
    and Release = 
        { ReleaseDate: DateTime
          ReleaseVersion: Version
          Security: bool
          CveList: CveEntry list
          ReleaseNotes: Url option
          Runtime: Runtime option
          Sdk: Sdk
          Sdks: Sdk list
          AspnetcoreRuntime: AspnetcoreRuntime option
          WindowsDesktop: WindowsDesktop option
          Symbols: Symbols option }

        static member Decoder : Decoder<Release> =
            Decode.object
                (fun get -> 
                    { ReleaseDate = get.Required.Field "release-date" Decode.datetime
                      ReleaseVersion = get.Required.Field "release-version" Decode.version
                      Security = get.Required.Field "security" Decode.bool
                      CveList = get.Optional.Field "cve-list" (Decode.list CveEntry.Decoder) |> Option.defaultValue []
                      ReleaseNotes = get.Optional.Field "release-notes" Decode.string
                      Runtime = get.Optional.Field "runtime" Runtime.Decoder
                      Sdk = get.Required.Field "sdk" Sdk.Decoder
                      Sdks = get.Optional.Field "sdks" (Decode.list Sdk.Decoder) |> Option.defaultValue []
                      AspnetcoreRuntime = get.Optional.Field "aspnetcore-runtime" AspnetcoreRuntime.Decoder
                      WindowsDesktop = get.Optional.Field "windowsdesktop" WindowsDesktop.Decoder
                      Symbols = get.Optional.Field "symbols" Symbols.Decoder })

    and CveEntry =
        { CveId: string
          CveUrl: Url }

        static member Decoder : Decoder<CveEntry> =
            Decode.object
                (fun get ->
                    { CveId = get.Required.Field "cve-id" Decode.string
                      CveUrl = get.Required.Field "cve-url" Decode.string })

    and Runtime =
        { Version: Version
          VersionDisplay: DisplayVersion option
          VsVersion: Version list
          VsMacVersion: Version list
          Files: File list }

        static member Decoder : Decoder<Runtime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VsVersion = get.Optional.Field "vs-version" (Decode.emptyStringAsNone (Decode.separatedString ',' Decode.version))
                                  |> Option.bind id
                                  |> Option.defaultValue []
                      VsMacVersion = get.Optional.Field "vs-mac-version" (Decode.emptyStringAsNone (Decode.separatedString ',' Decode.version))
                                     |> Option.bind id
                                     |> Option.defaultValue []
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and Sdk = 
        { Version: Version
          VersionDisplay: DisplayVersion option
          RuntimeVersion: Version option
          VsVersion: Version list
          VsMacVersion: Version option
          VsSupport: string option
          VsMacSupport: string option
          CsharpVersion: Version option
          FsharpVersion: Version option
          VbVersion: Version option
          Files: File list }

        static member Decoder : Decoder<Sdk> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      RuntimeVersion = get.Optional.Field "runtime-version" Decode.version
                      VsVersion = get.Optional.Field "vs-version" (Decode.emptyStringAsNone (Decode.separatedString ',' Decode.version))
                                  |> Option.bind id
                                  |> Option.defaultValue []
                      VsMacVersion = get.Optional.Field "vs-mac-version" (Decode.emptyStringAsNone Decode.version) |> Option.bind id
                      VsSupport = get.Optional.Field "vs-support" Decode.string
                      VsMacSupport = get.Optional.Field "vs-mac-support" Decode.string
                      CsharpVersion = get.Optional.Field "csharp-version" (Decode.emptyStringAsNone Decode.version) |> Option.bind id
                      FsharpVersion = get.Optional.Field "fsharp-version" (Decode.emptyStringAsNone Decode.version) |> Option.bind id
                      VbVersion = get.Optional.Field "vb-version" (Decode.emptyStringAsNone Decode.version) |> Option.bind id
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and AspnetcoreRuntime =
        { Version: Version
          VersionDisplay: DisplayVersion option
          VersionAspnetcoremodule: Version list
          VsVersion: Version list
          Files: File list }

        static member Decoder : Decoder<AspnetcoreRuntime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VersionAspnetcoremodule = 
                        get.Optional.Field "version-aspnetcoremodule" (Decode.list Decode.version) 
                        |> Option.defaultValue []
                      VsVersion = get.Optional.Field "vs-version" (Decode.emptyStringAsNone (Decode.separatedString ',' Decode.version))
                                  |> Option.bind id
                                  |> Option.defaultValue []
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and WindowsDesktop =
        { Version: Version
          VersionDisplay: DisplayVersion option
          Files: File list }

        static member Decoder : Decoder<WindowsDesktop> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and Symbols =
        { Version: Version
          Files: File list }

        static member Decoder : Decoder<Symbols> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.version
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    and File =
        { Name: string
          Rid: string option
          Url: Url
          Hash: string }

        static member Decoder : Decoder<File> =
            Decode.object
                (fun get ->
                    { Name = get.Required.Field "name" Decode.string
                      Rid = get.Optional.Field "rid" Decode.string
                      Url = get.Required.Field  "url" Decode.string
                      Hash = get.Required.Field "hash" Decode.string })
