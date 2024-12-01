module AdventOfCode2024.Day1

open System
open System.IO
open FSharpPlus

let run() =
    let lines = File.ReadAllLines "./data1.txt"

    let numbers = lines |> map(String.split [" "] >> Seq.except [""] >> Seq.map int >> toList)
    let firsts = numbers |> map(Seq.last)
    let seconds = numbers |> map(Seq.head)

    let result1 = Seq.zip (sort firsts) (sort seconds) |> map(fun (a, b) -> Math.Abs(a - b)) |> sum
        
    let result2 =
        let counts = seconds |> Seq.countBy id |> Map
        firsts |> Seq.map (fun number -> (counts.TryFind number |> Option.defaultValue 0) * number) |> sum

    printfn "%d %d" result1 result2
