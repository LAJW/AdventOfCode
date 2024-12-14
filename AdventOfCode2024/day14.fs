module AdventOfCode2024.day14

// #r "./bin/Debug/net8.0/AdventOfCode2024.dll"
// #r "nuget: FSharpPlus"
open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2024.Utils
open System.Threading

let parse (line: string) =
    Regex.Match(line, @"p=([-\d]+),([-\d]+) v=([-\d]+),([-\d]+)").Groups
    |> Seq.toList
    |> List.map _.Value
    |> function
        | [ _; px; py; vx; vy ] -> struct (Vec(int px, int py), Vec(int vx, int vy))
        | o -> failwith ("Unreachable" + o.ToString())

let adjust bound value = ((value % bound) + bound) % bound

let part1 () =
    let lines = File.ReadAllLines("./day14.txt")
    let width = 101
    let height = 103

    let bots =
        lines
        |> Seq.map parse
        |> Seq.map (fun struct (pos, velocity) ->
            let total = pos + velocity * 100
            Vec(adjust width total.X, adjust height total.Y))
        |> Seq.countBy id
        |> Map

    let middleX = width / 2
    let middleY = height / 2

    let botsPerQuadrant =
        [ Seq.allPairs (seq { 0 .. middleX - 1 }) (seq { 0 .. middleY - 1 })
          Seq.allPairs (seq { middleX + 1 .. width - 1 }) (seq { 0 .. middleY - 1 })
          Seq.allPairs (seq { 0 .. middleX - 1 }) (seq { middleY + 1 .. height - 1 })
          Seq.allPairs (seq { middleX + 1 .. width - 1 }) (seq { middleY + 1 .. height - 1 }) ]
        |> Seq.map (fun pairs -> pairs |> Seq.map Vec)
        |> Seq.map (fun quadrant -> quadrant |> Seq.choose bots.TryFind)

    let safetyFactor = botsPerQuadrant |> Seq.map Seq.sum |> Seq.fold (*) 1
    printfn "%d" safetyFactor

let part2 () =
    let lines = File.ReadAllLines("./day14.txt")
    let width = 101
    let height = 103
    let mutable bots = lines |> Seq.map parse
    let mutable wait = false

    let update (count: int) =
        bots <-
            bots
            |> Seq.map (fun struct (pos, velocity) ->
                let total = pos + velocity * count
                struct (Vec(adjust width total.X, adjust height total.Y), velocity))

    Thread(fun () ->
        update 4

        for i in 1..10000 do
            while wait do
                Thread.Sleep(1)

            let map = bots |> Seq.map sfst |> Set
            printfn "================================================================================"
            printfn "ITERATION %d" i
            printfn "================================================================================"

            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    if map.Contains(Vec(x, y)) then printf "#" else printf "."

                printfn ""

            Thread.Sleep(100)
            update 101)
        .Start()

    while true do
        Console.ReadKey() |> ignore
        wait <- not wait
    ()
