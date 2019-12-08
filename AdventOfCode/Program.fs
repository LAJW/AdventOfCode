﻿open System.IO
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
    |> Day3.intersectionSteps
    |> printfn "%d"

let day4() =
    Day4.findAllValidInRage Day4.isValid2 171309 643603
    |> printfn "%d"

let day5Part1() =
    File.ReadLines("input5.txt")
    |> Seq.head
    |> Day2.deserialize
    |> Day5.run [1]
    |> Seq.map string
    |> String.join ","
    |> printfn "%s"

let day5Part2() =
    File.ReadLines("input5.txt")
    |> Seq.head
    |> Day2.deserialize
    |> Day5.run [5]
    |> Seq.map string
    |> String.join ","
    |> printfn "%s"

let day6Part1() =
    File.ReadLines("input6.txt")
    |> Day6.parse
    |> Day6.countOrbits
    |> printfn "%d"

let day6Part2() =
    File.ReadLines("input6.txt")
    |> Day6.parse
    |> Day6.distance "YOU" "SAN"
    |> printfn "%d"


let day7Part1() =
    File.ReadLines("input7.txt")
    |> Seq.head
    |> Day5.deserialize
    |> Day7.findHighestOutput
    |> printfn "%d"



let day8Part1() =
    File.ReadLines("input8.txt")
    |> Seq.head
    |> Seq.map (fun char -> char.ToString() |> int)
    |> Seq.chunkBySize (25 * 6)
    |> Seq.minBy (Seq.filter ((=) 0) >> Seq.length)
    |> Seq.countBy id
    |> Seq.sortBy fst
    |> Seq.skip 1
    |> Seq.take 2
    |> Seq.map snd
    |> Seq.toList
    |> (function [a; b] -> a * b)
    |> printfn "%d"

[<EntryPoint>]
let main argv =
    day8Part1()
    0
