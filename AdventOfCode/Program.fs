open System.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Extensions

let day2() =
    File.ReadLines("input2.txt")
    |> Seq.head
    |> Day2.deserialize
    |> Day2.findArgumentsFor 19690720
    |> Seq.map(fun tuple -> tuple.ToString())
    |> String.join "|"
    |> printfn "%s"

let day3() =
    File.ReadLines("input3.txt")
    |> Seq.toList
    |> Day3.distance
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    day3()
    0
