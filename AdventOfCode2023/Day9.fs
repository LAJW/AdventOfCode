open System
open FSharpPlus
open System.IO

let data = File.ReadAllLines "./data9.txt"

let rows = data |> Seq.map (String.split [" "] >> map Int32.Parse >> toList) |> toList

let proc = Seq.pairwise >> map (fun (a, b) -> b - a) >> toList

let solve postProc =
    rows |> Seq.map (fun row ->
        (row, (Seq.init row.Length id)) ||> Seq.scan (fun state _ -> proc state)
        |> takeWhile (exists ((<>) 0))
        |> postProc
    ) |> sum

solve (map Seq.last >> sum) |> printfn "Part 1: %d"
solve (map head >> Seq.reduceBack (-)) |> printfn "Part 2: %d"
