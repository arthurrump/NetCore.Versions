namespace VersionsOfDotNet

open System
open FSharp.Control.Tasks.V2.ContextInsensitive

open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open GitHub.Webhook

module Server =
    let appId         = Environment.GetEnvironmentVariable("GITHUB_APP_ID") |> Int32.Parse
    let webhookSecret = Environment.GetEnvironmentVariable("GITHUB_WEBHOOK_SECRET")
    let privateKey    = Environment.GetEnvironmentVariable("GITHUB_PRIVATE_KEY")

    let jwtGenerator = GitHub.jwtGenerator appId privateKey

    let handleWebhook : HttpHandler =
        fun next ctx -> task {
            match! tryGetWebhookRequest webhookSecret ctx with
            | CheckSuiteEvent (CheckSuiteAction.Requested,   event)
            | CheckSuiteEvent (CheckSuiteAction.Rerequested, event)
            | CheckRunEvent   (CheckRunAction  .Rerequested, event) -> 
                let jwt = jwtGenerator.CreateEncodedJwtToken()
                let appClient = GitHub.appClient jwt
                let! installationClient = GitHub.installationClient appClient event.InstallationId
                Run.runChecks 
                    installationClient
                    event.RepoOwner
                    event.RepoName
                    event.CommitHash
                    |> Async.Start
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
