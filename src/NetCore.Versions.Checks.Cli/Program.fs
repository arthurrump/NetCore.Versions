module NetCore.Versions.Checks.Cli

[<EntryPoint>]
let main argv =
    match argv with
    | [| owner; repo; hash |] -> Run.runLocal owner repo hash |> Async.RunSynchronously
    | _ -> printfn "Arguments: owner repo hash"
    0 
