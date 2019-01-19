namespace VersionsOfDotNet

open System
open Thoth.Json.Net
open System.Net.Http
open System.Net
open JWT.Builder
open JWT.Algorithms
open System.Security.Cryptography.X509Certificates
open System.Security.Cryptography
open System.Text
open System.Net.Http.Headers
open JWT
open System.Security.Cryptography

module GitHub =
    type CheckRun =
        { Name: string
          HeadSha: string
          Status: CheckRunStatus
          Output: CheckRunOutput option }

        member this.Encode() =
            Encode.object <|
                [ yield "name", Encode.string this.Name
                  yield "head-sha", Encode.string this.HeadSha
                  match this.Output with 
                  | Some output -> yield "output", output.Encode() 
                  | None -> () ] 
                @ this.Status.Encode()

    and CheckRunStatus =
        | Queued
        | InProgress
        | Completed of CheckRunConclusion * completedAt: DateTimeOffset

        member this.Encode() =
            match this with
            | Queued -> [ "status", Encode.string "queued" ]
            | InProgress -> [ "status", Encode.string "in_progress" ]
            | Completed (conclusion, completedAt) ->
                [ "status", Encode.string "completed"
                  "conclusion", conclusion.Encode()
                  "completed_at", Encode.datetime completedAt.UtcDateTime ]
    
    and CheckRunConclusion =
        | Success
        | Failure
        | Neutral
        | Cancelled
        | TimedOut

        member this.Encode() =
            match this with
            | Success -> Encode.string "success"
            | Failure -> Encode.string "failure"
            | Neutral -> Encode.string "neutral"
            | Cancelled -> Encode.string "cancelled"
            | TimedOut -> Encode.string "timed_out"

    and CheckRunOutput = 
        { Title: string
          Summary: string
          Text: string }

        member this.Encode() =
            Encode.object
                [ "title", Encode.string this.Title
                  "summary", Encode.string this.Summary
                  "text", Encode.string this.Text ]

    type CheckSuiteEvent =
        { Action: CheckSuiteAction
          CommitHash: string
          RepoFullName: string }

        static member Decoder : Decode.Decoder<CheckSuiteEvent> =
            Decode.object (fun get ->
                { Action = get.Required.Field "action" CheckSuiteAction.Decoder
                  CommitHash = get.Required.At [ "check_suite"; "head_sha" ] Decode.string
                  RepoFullName = get.Required.At [ "repository"; "full_name" ] Decode.string })

    and CheckSuiteAction =
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

    type CheckRunEvent =
        { Action: CheckRunAction
          CommitHash: string
          RepoFullName: string }
        
        static member Decoder : Decode.Decoder<CheckRunEvent> =
            Decode.object (fun get ->
                { Action = get.Required.Field "action" CheckRunAction.Decoder
                  CommitHash = get.Required.At [ "check_run"; "head_sha" ] Decode.string
                  RepoFullName = get.Required.At [ "repository"; "full_name" ] Decode.string })

    and CheckRunAction =
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

    let url parts =
        "https://api.github.com/" + (parts |> String.concat "/")

    type Error =
        | JsonParseError of string
        | HttpError of HttpStatusCode

    let postNewChecksRun (http : HttpClient) repo (run: CheckRun) = async {
        let url = url [ repo; "check-runs" ]
        let! resp = http.PostAsync(url, new StringContent(run.Encode() |> Encode.toString 0)) |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! content = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let id = Decode.fromString (Decode.field "id" Decode.int) content
            match id with
            | Ok id -> return Ok id
            | Error e -> return Error (JsonParseError e)
         else
            return Error (HttpError resp.StatusCode)
    }

    let updateChecksRun (http : HttpClient) repo id (run : CheckRun) = async {
        let url = url [ repo; "check-runs"; string id ]
        use req = new HttpRequestMessage()
        req.Method <- HttpMethod("PATCH")
        req.RequestUri <- Uri(url)
        req.Content <- new StringContent(run.Encode() |> Encode.toString 0)
        let! resp = http.SendAsync(req) |> Async.AwaitTask
        if resp.IsSuccessStatusCode 
        then return Ok ()
        else return Error (HttpError resp.StatusCode)
    }

    type RS256PrivateKeyAlgorithm(key : RSA) =
        interface IJwtAlgorithm with
            member __.Name = JwtHashAlgorithm.RS256.ToString()
            member __.IsAsymmetric = true
            member __.Sign(_, bytesToSign : byte[]) =
                key.SignData(bytesToSign, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)

    let getAccessToken (http : HttpClient) (privateKey : string) appId installationId = async {
        let now = DateTime.UtcNow
        use key = CngKey.Import([||], CngKeyBlobFormat.Pkcs8PrivateBlob) // TODO fix
        let jwt = 
            JwtBuilder()
                .Issuer(appId)
                .IssuedAt(now)
                .ExpirationTime(now.AddMinutes(2.))
                .WithAlgorithm(RS256Algorithm(cert))
                .Build()
        let url = url [ "app"; "installations"; installationId; "access_tokens" ]
        http.DefaultRequestHeaders.Authorization <- AuthenticationHeaderValue("Bearer", jwt)
        let! resp = http.PostAsync(url, new StringContent("")) |> Async.AwaitTask
        if resp.IsSuccessStatusCode then
            let! data = resp.Content.ReadAsStringAsync() |> Async.AwaitTask
            let token = data |> Decode.fromString (Decode.field "token" Decode.string)
            match token with
            | Ok t -> return Ok t
            | Error e -> return Error (JsonParseError e)
        else return Error (HttpError resp.StatusCode)
    }