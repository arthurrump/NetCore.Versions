namespace VersionsOfDotNet

open Thoth.Json
open System

module Data =
    type Url = string
    type Version = string

    let private getOptionalDate (get: Decode.IGetters) jsonName =
        get.Required.Field jsonName Decode.string
        |> fun s -> if String.IsNullOrWhiteSpace s then (false, DateTime()) else DateTime.TryParse(s)
        |> fun (s, d) -> if s then Some d else None

    type IndexEntry =
        { ChannelVersion: Version
          LatestRelease: Version
          Product: string
          SupportPhase: string
          EolDate: DateTime option
          ReleasesJson: Url }

        static member Decoder : Decode.Decoder<IndexEntry> = 
            Decode.object
                (fun get -> 
                    { ChannelVersion = get.Required.Field "channel-version" Decode.string
                      LatestRelease = get.Required.Field "latest-release" Decode.string
                      Product = get.Required.Field "product" Decode.string
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = getOptionalDate get "eol-date"
                      ReleasesJson = get.Required.Field "releases.json" Decode.string })

    type File =
        { Name: string
          Url: Url
          Hash: string }

        static member Decoder : Decode.Decoder<File> =
            Decode.object
                (fun get ->
                    { Name = get.Required.Field "name" Decode.string
                      Url = get.Required.Field  "url" Decode.string
                      Hash = get.Required.Field "hash" Decode.string })

    type Runtime =
        { Version: Version
          VersionDisplay: Version option
          VsVersion: Version option
          Files: File list }

        static member Decoder : Decode.Decoder<Runtime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.string
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VsVersion = get.Optional.Field "vs-version" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    type Sdk = 
        { Version: Version
          VersionDisplay: Version option
          VsVersion: Version option
          CsharpLanguage: Version option
          FsharpLanguage: Version option
          Files: File list }

        static member Decoder : Decode.Decoder<Sdk> =
            Decode.object
                (fun get ->
                    { Version = match get.Optional.Field "version" Decode.string with
                                | Some v -> v
                                | None -> get.Required.Field "version-sdk" Decode.string
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VsVersion = get.Optional.Field "vs-version" Decode.string
                      CsharpLanguage = get.Optional.Field "csharp-language" Decode.string
                      FsharpLanguage = get.Optional.Field "fsharp-language" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    type AspnetcoreRuntime =
        { Version: Version
          VersionDisplay: Version option
          VersionAspnetcoremodule: Version list option
          Files: File list }

        static member Decoder : Decode.Decoder<AspnetcoreRuntime> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.string
                      VersionDisplay = get.Optional.Field "version-display" Decode.string
                      VersionAspnetcoremodule = get.Optional.Field "version-aspnetcoremodule" (Decode.list Decode.string)
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    type Symbols =
        { Version: Version
          Files: File list }

        static member Decoder : Decode.Decoder<Symbols> =
            Decode.object
                (fun get ->
                    { Version = get.Required.Field "version" Decode.string
                      Files = get.Required.Field "files" (Decode.list File.Decoder) })

    type Release = 
        { ReleaseDate: DateTime
          ReleaseVersion: Version option
          Security: bool
          ReleaseNotes: Url option
          Runtime: Runtime option
          Sdk: Sdk
          AspnetcoreRuntime: AspnetcoreRuntime option
          Symbols: Symbols option }

        static member Decoder : Decode.Decoder<Release> =
            Decode.object
                (fun get -> 
                    { ReleaseDate = get.Required.Field "release-date" Decode.datetime
                      ReleaseVersion = get.Optional.Field "release-version" Decode.string
                      Security = get.Required.Field "security" Decode.bool
                      ReleaseNotes = get.Optional.Field "release-notes" Decode.string
                      Runtime = get.Optional.Field "runtime" Runtime.Decoder
                      Sdk = get.Required.Field "sdk" Sdk.Decoder
                      AspnetcoreRuntime = get.Optional.Field "aspnetcore-runtime" AspnetcoreRuntime.Decoder
                      Symbols = get.Optional.Field "symbols" Symbols.Decoder })

    type Channel =
        { ChannelVersion: Version
          LatestRelease: Version
          LatestReleaseDate: DateTime
          SupportPhase: string
          EolDate: DateTime option
          LifecyclePolicy: Url
          Releases: Release list }

        static member Decoder : Decode.Decoder<Channel> =
            Decode.object
                (fun get ->
                    { ChannelVersion = get.Required.Field "channel-version" Decode.string
                      LatestRelease = get.Required.Field "latest-release" Decode.string
                      LatestReleaseDate = get.Required.Field "latest-release-date" Decode.datetime
                      SupportPhase = get.Required.Field "support-phase" Decode.string
                      EolDate = getOptionalDate get "eol-date"
                      LifecyclePolicy = get.Required.Field "lifecycle-policy" Decode.string
                      Releases = get.Required.Field "releases" (Decode.list Release.Decoder) })