namespace VersionsOfDotNet

[<RequireQualifiedAccess>]
module internal Async =
    let map fn asyncThing =
        async {
            let! thing = asyncThing
            return fn thing
        }

[<AutoOpen>]
module internal Helpers =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then Some(s.Substring(p.Length))
        else None