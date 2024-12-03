module AdventOfCode2024.day3

open System.Text.RegularExpressions
open System.IO

let run1 () =
    let text = File.ReadAllText "./data3.txt"

    Regex.Matches(text, @"mul\((\d+),(\d+)\)")
    |> Seq.sumBy (fun m ->
        match m.Groups |> Seq.toList with
        | [ _; a; b ] -> int a.Value * int b.Value
        | _ -> failwith "unreachable"
    )
    |> printfn "%d"

let run2 () =
    let text = File.ReadAllText "./data3.txt"

    Regex.Matches(text, @"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
    |> Seq.map (fun m -> m.Groups |> Seq.map (_.Value) |> Seq.toList)
    |> Seq.fold (fun (sum, enabled) matches ->
        match matches with
        | "do()" :: _ -> (sum, true)
        | "don't()" :: _ -> (sum, false)
        | [_; a; b] when enabled -> (sum + int a * int b, enabled)
        | _ -> (sum, enabled)
    ) (0, true)
    |> fst
    |> printfn "%d"
