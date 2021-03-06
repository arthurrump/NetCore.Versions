namespace NetCore.Versions.Checks

open System
open FSharp.Control.Tasks.V2.ContextInsensitive

open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging

open GitHub.Webhook

module Server =
    let appId         = Environment.GetEnvironmentVariable("GITHUB_APP_ID") |> Int32.Parse
    let webhookSecret = Environment.GetEnvironmentVariable("GITHUB_WEBHOOK_SECRET")
    let privateKey    = Environment.GetEnvironmentVariable("GITHUB_PRIVATE_KEY")

    let jwtGenerator = GitHub.jwtGenerator appId privateKey

    let handleWebhook : HttpHandler =
        fun next ctx -> task {
            let logger = ctx.GetLogger("Webhook")
            logger.LogInformation("Webhook requested")
            match! tryGetWebhookRequest webhookSecret ctx with
            | CheckSuiteEvent  (CheckSuiteAction .Requested,   event)
            | CheckSuiteEvent  (CheckSuiteAction .Rerequested, event)
            | CheckRunEvent    (CheckRunAction   .Rerequested, event)
            | PullRequestEvent (PullRequestAction.Opened,      event)
            | PullRequestEvent (PullRequestAction.Edited,      event)
            | PullRequestEvent (PullRequestAction.Reopened,    event)
            | PullRequestEvent (PullRequestAction.Synchronize, event) ->
                logger.LogInformation("New CheckRun requested by webhook")
                let jwt = jwtGenerator.CreateEncodedJwtToken()
                let appClient = GitHub.appClient jwt
                let! installationClient = 
                    try GitHub.installationClient appClient event.InstallationId
                    with ex ->
                        logger.LogError(ex, "An error occured while creating a GitHub installation client")
                        raise ex
                Run.runChecks
                    (ctx.GetLogger("Checks"))
                    installationClient
                    event.RepoOwner
                    event.RepoName
                    event.CommitHash
                    |> Async.Start
                return! Successful.OK 
                    (sprintf "Running checks for %s/%s %s" event.RepoOwner event.RepoName, event.CommitHash) 
                    next ctx
            | CheckSuiteEvent _ | CheckRunEvent _ | PullRequestEvent _ ->
                logger.LogInformation("Valid request, no action required.")
                return! Successful.OK "Valid request, no action required." next ctx
            | Ping ->
                logger.LogInformation("Ping Webhook request")
                return! Successful.OK "Ping" next ctx
            | Invalid msg ->
                logger.LogInformation("Invalid Webhook request: {0}", msg)
                return! RequestErrors.BAD_REQUEST msg next ctx
        }

    let webApp =
        choose [ 
            GET >=> redirectTo true "https://github.com/apps/netcore-versions-checks"
            POST >=> route "/hook" >=> handleWebhook
            RequestErrors.NOT_FOUND "Not found."
        ]

    let errorHandler (ex : Exception) (logger : ILogger) =
        logger.LogError(ex, "An unhandled exception occurred.")
        clearResponse
        >=> ServerErrors.INTERNAL_ERROR "An unhandled exception occured."

    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffeErrorHandler(errorHandler)
           .UseGiraffe(webApp)

    let configureServices (services : IServiceCollection) =
        services.AddGiraffe() |> ignore

    let configureLogging (builder : ILoggingBuilder) =
        builder.AddConsole()
        |> ignore

    [<EntryPoint>]
    let main _ =
        WebHostBuilder()
            .UseKestrel()
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)
            .ConfigureLogging(configureLogging)
            .Build()
            .Run()
        0
