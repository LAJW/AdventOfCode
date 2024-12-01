module AdventOfCode2024.day2

open System
open System.IO
open FSharpPlus

let run() =
    let lines = File.ReadAllLines "./data2.txt"

    let numbers = lines |> map(String.split [" "] >> Seq.except [""] >> Seq.map int >> toList)

    printfn "%d" result1
