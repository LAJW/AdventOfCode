module FSharpPlayground
open FSharpPlus
open System.IO

let day1_1() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.max
    |> printfn "%d"
    
let day1_2() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%d"

[<EntryPoint>]
let main _argv =
    day1_2()
    0
