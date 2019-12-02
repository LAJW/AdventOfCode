open System.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Extensions

[<EntryPoint>]
let main argv =
    File.ReadLines("input2.txt")
    |> Seq.head
    |> Day2.deserialize
    |> Day2.findArgumentsFor 19690720
    |> Seq.map(fun tuple -> tuple.ToString())
    |> String.join "|"
    |> printfn "%s"
    0
