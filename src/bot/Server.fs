namespace VersionsOfDotNet

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Giraffe

open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Security.Cryptography
open System.Text
open GitHub
open Thoth.Json.Net

module Server =
    let webhookSecret = Environment.GetEnvironmentVariable("GITHUB_WEBHOOK_SECRET")
    let appId = Environment.GetEnvironmentVariable("GITHUB_APP_ID")
    let privateKey = Environment.GetEnvironmentVariable("GITHUB_PRIVATE_KEY")

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then Some(s.Substring(p.Length))
        else None

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

    type GitHubRequest =
        | CheckSuiteEvent of CheckSuiteEvent
        | CheckRunEvent of CheckRunEvent
        | Ping
        | Invalid

    let tryGetGithubRequest (ctx : HttpContext) = task {
        let delivery = ctx.TryGetRequestHeader "X-GitHub-Delivery"
        let userAgent = ctx.TryGetRequestHeader "User-Agent"
        let signature = ctx.TryGetRequestHeader "X-Hub-Signature"
        let event = ctx.TryGetRequestHeader "X-GitHub-Event"

        if validDeliveryHeader delivery && validUserAgent userAgent then
            let! body = ctx.ReadBodyFromRequestAsync ()
            if validSignature webhookSecret signature body then
                match event with
                | Some "check_suite" -> 
                    match body |> Decode.fromString CheckSuiteEvent.Decoder with
                    | Ok cse -> return CheckSuiteEvent cse
                    | Error _ -> return Invalid
                | Some "check_run" -> 
                    match body |> Decode.fromString CheckRunEvent.Decoder with
                    | Ok cre -> return CheckRunEvent cre
                    | Error _ -> return Invalid
                | Some "ping" -> return Ping
                | Some _ | None -> return Invalid
            else return Invalid
        else return Invalid
    }

    let handleWebhook : HttpHandler =
        fun next ctx -> task {
            match! tryGetGithubRequest ctx with
            | CheckSuiteEvent { Action = Requested; CommitHash = hash; RepoFullName = repo } 
            | CheckSuiteEvent { Action = CheckSuiteAction.Rerequested; CommitHash = hash; RepoFullName = repo }
            | CheckRunEvent { Action = Rerequested; CommitHash = hash; RepoFullName = repo } -> 
                Run.runChecks repo hash |> Async.Start
                return! Successful.NO_CONTENT next ctx
            | Ping ->
                return! Successful.OK "ping" next ctx
            | Invalid | _ ->
                return! RequestErrors.BAD_REQUEST "Invalid request." next ctx
        }

    let webApp =
        choose [ 
            GET >=> redirectTo true "https://github.com/apps/versionsof-net-checks"
            POST >=> route "/hook" >=> handleWebhook
            RequestErrors.NOT_FOUND "Not found."
        ]

    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe webApp

    let configureServices (services : IServiceCollection) =
        services.AddGiraffe() |> ignore

    [<EntryPoint>]
    let main _ =
        WebHostBuilder()
            .UseKestrel()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .Build()
            .Run()
        0
