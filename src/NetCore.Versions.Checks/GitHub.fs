namespace NetCore.Versions.Checks

open FSharp.Control.Tasks.V2.ContextInsensitive

open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

open System.Text
open System.Security.Cryptography

open Thoth.Json.Net

open GitHubJwt
open Octokit

module GitHub =
    module Webhook = 
        type Event =
            { CommitHash: string
              RepoOwner: string
              RepoName: string
              InstallationId: int64 }

            static member Decoder commitHash : Decode.Decoder<Event> =
                Decode.object (fun get ->
                    { CommitHash = get.Required.At commitHash Decode.string
                      RepoOwner = get.Required.At [ "repository"; "owner"; "login" ] Decode.string 
                      RepoName = get.Required.At [ "repository"; "name" ] Decode.string
                      InstallationId = get.Required.At [ "installation"; "id" ] Decode.int64 })

        type CheckSuiteAction =
            | Completed
            | Requested
            | Rerequested

            static member Decoder path value =
                let str = Decode.string path value
                match str with
                | Ok "completed" -> Ok Completed
                | Ok "requested" -> Ok Requested
                | Ok "rerequested" -> Ok Rerequested
                | Ok _ -> (path, Decode.BadPrimitive("a valid action", value)) |> Error
                | Error e -> Error e

        type CheckRunAction =
            | Created
            | Rerequested
            | RequestedAction

            static member Decoder path value =
                let str = Decode.string path value
                match str with
                | Ok "created" -> Ok Created
                | Ok "rerequested" -> Ok Rerequested
                | Ok "requested_action" -> Ok RequestedAction
                | Ok _ -> (path, Decode.BadPrimitive("a valid action", value)) |> Error
                | Error e -> Error e

        type PullRequestAction =
            | Opened
            | Reopened
            | Edited
            | Closed
            | Other of string

            static member Decoder path value =
                let str = Decode.string path value
                match str with
                | Ok "opened" -> Ok Opened
                | Ok "reopened" -> Ok Reopened
                | Ok "edited" -> Ok Edited
                | Ok "closed" -> Ok Closed
                | Ok action -> Ok (Other action)
                | Error e -> Error e

        let validDeliveryHeader = Option.isSome

        let validUserAgent = Option.exists <| fun (h : string) -> h.StartsWith "GitHub-Hookshot/"

        let validSignature (secret : string) (header : string option) (body : string) =
            match header with
            | Some (Prefix "sha1=" signature) ->
                let bodyBytes = Encoding.ASCII.GetBytes body
                let secretBytes = Encoding.ASCII.GetBytes secret

                use sha1 = new HMACSHA1(secretBytes) // DevSkim: ignore DS126858 
                let hash = 
                    sha1.ComputeHash bodyBytes 
                    |> Array.map (fun b -> b.ToString("x2"))
                    |> String.concat ""
                
                hash = signature
            | Some _ | None -> false

        type Request =
            | CheckSuiteEvent of CheckSuiteAction * Event
            | CheckRunEvent of CheckRunAction * Event
            | PullRequestEvent of PullRequestAction * Event
            | Ping
            | Invalid of reason : string

        let tryGetWebhookRequest webhookSecret (ctx : HttpContext) = task {
            let delivery = ctx.TryGetRequestHeader "X-GitHub-Delivery"
            let userAgent = ctx.TryGetRequestHeader "User-Agent"
            let signature = ctx.TryGetRequestHeader "X-Hub-Signature"
            let event = ctx.TryGetRequestHeader "X-GitHub-Event"

            if validDeliveryHeader delivery && validUserAgent userAgent then
                let! body = ctx.ReadBodyFromRequestAsync ()
                if validSignature webhookSecret signature body then
                    match event with
                    | Some "check_suite" ->
                        match body |> Decode.fromString (Event.Decoder [ "check_suite"; "head_sha" ]) with
                        | Ok event -> 
                            match body |> Decode.fromString (Decode.field "action" CheckSuiteAction.Decoder) with
                            | Ok action -> return CheckSuiteEvent (action, event)
                            | Error msg -> return Invalid msg
                        | Error msg -> return Invalid msg
                    | Some "check_run" -> 
                        match body |> Decode.fromString (Event.Decoder [ "check_run"; "head_sha" ]) with
                        | Ok event -> 
                            match body |> Decode.fromString (Decode.field "action" CheckRunAction.Decoder) with
                            | Ok action -> return CheckRunEvent (action, event)
                            | Error msg -> return Invalid msg
                        | Error msg -> return Invalid msg
                    | Some "pull_request" ->
                        match body |> Decode.fromString (Event.Decoder [ "pull_request"; "head"; "sha" ]) with
                        | Ok event ->
                            match body |> Decode.fromString (Decode.field "action" PullRequestAction.Decoder) with
                            | Ok action -> return PullRequestEvent (action, event)
                            | Error msg -> return Invalid msg
                        | Error msg -> return Invalid msg
                    | Some "ping" -> return Ping
                    | Some ev -> return Invalid (sprintf "Event: %s" ev) 
                    | None -> return Invalid "No event header."
                else
                    return Invalid "Signature."
            else
                return Invalid (sprintf "No delivery header or invalid user-agent: %A" userAgent)
        }

    let jwtGenerator appId privateKey =
        GitHubJwtFactory(
            StringPrivateKeySource(privateKey),
            GitHubJwtFactoryOptions(
                AppIntegrationId = appId,
                ExpirationSeconds = 120))

    let appClient jwt =
        GitHubClient(
            ProductHeaderValue("netcore-versions-checks"),
            Credentials = Credentials(jwt, AuthenticationType.Bearer))

    let installationClient (appClient : GitHubClient) installationId = task {
        let! token = appClient.GitHubApps.CreateInstallationToken(installationId)
        return GitHubClient(
            ProductHeaderValue("netcore-versions-checks-installation-" + string installationId),
            Credentials = Credentials(token.Token))
    }
