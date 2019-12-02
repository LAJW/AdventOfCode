open System.IO
open FSharp.Data.UnitSystems.SI.UnitSymbols

[<EntryPoint>]
let main argv =
    File.ReadLines("input2.txt")
    |> Seq.head
    |> Day2.func2
    |> printfn "%s"
    0
