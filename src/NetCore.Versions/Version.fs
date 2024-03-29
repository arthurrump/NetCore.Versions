namespace NetCore.Versions

open System

#nowarn "342" // Don't warn about implementing Equals

[<CustomComparison; StructuralEquality>]
[<StructuredFormatDisplay("{String}")>] // Prints as a normal version string when "%A" is used
type Version =
    { Numbers: int list
      Preview: string option }

    // StructuredFormatDisplay only works with properties
    member private this.String =
        let s = this.Numbers |> List.map string |> String.concat "."
        match this.Preview with
        | Some preview -> sprintf "%s%s" s preview
        | None -> s

    override this.ToString() = 
        this.String

    member this.CompareTo(other) =
        match compare this.Numbers other.Numbers with
        | 0 -> 
            match this.Preview, other.Preview with
            | None, None -> 0
            | Some p1, Some p2 -> compare p1 p2
            | Some _, None -> -1
            | None, Some _ -> 1
        | c -> c

    interface IComparable with
        member this.CompareTo(other: obj) =
            match other with
            | :? Version as v -> this.CompareTo(v)
            | _ -> invalidArg "other" "cannot compare values of different types"

module Version =
    module private Int =
        let parse (i : string) = 
            match Int32.TryParse i with
            | (true, r) -> Some r
            | _ -> None

    module private String =
        let trim (s: string) = s.Trim()

    let parse (s: string) =
        let rec parseNumbers numbersRev (str: char list) =
            match str with
            | ch::_ when Char.IsDigit ch ->
                Seq.takeWhile Char.IsDigit str
                |> Seq.toArray 
                |> String 
                |> Int.parse
                |> Option.bind (fun i -> parseNumbers (i::numbersRev) (str |> List.skipWhile Char.IsDigit))
            | '.'::str -> 
                parseNumbers numbersRev str
            | _ when not (List.isEmpty numbersRev) -> 
                Some (List.rev numbersRev, str)
            | _ ->
                None
        String.trim s 
        |> Seq.toList 
        |> parseNumbers []
        |> Option.map (fun (numbers, rest) ->
            { Numbers = 
                numbers
              Preview = 
                if List.isEmpty rest
                then None
                else Some (String (List.toArray rest)) })

    let displayedAs display (version: Version) =
        string version = display

    let mightMatchInChannel channelVersion version =
        List.tryHead channelVersion.Numbers = List.tryHead version.Numbers

    let matches versionStart version =
        versionStart.Numbers.Length <= version.Numbers.Length &&
        versionStart.Numbers = List.take versionStart.Numbers.Length version.Numbers &&
        match versionStart.Preview with 
        | Some p1 -> 
            match version.Preview with
            | Some p2 -> p2.StartsWith(p1)
            | None -> false
        | None -> true

    let (|Version|_|) input = parse input

    let isPreview version =
        version.Preview.IsSome
