module AdventOfCode2024.day10

// #r "nuget:FSharpPlus"
// #r "./bin/Debug/net8.0/AdventOfCode2024.dll"
open FSharpPlus
open System
open System.IO
open System.Collections.Generic
open AdventOfCode2024.Utils


let step (pos: Vec) (grid: Grid<int8>) =
    let cardinals = [ Vec(1, 0); Vec(0, 1); Vec(-1, 0); Vec(0, -1) ]
    cardinals |> Seq.map ((+) pos) |> Seq.filter grid.HasIndex

let run1() =
    let lines =
        File.ReadAllLines("day10.txt")
        |> Array.map (String.toArray >> Array.map (fun x -> x - '0' |> int8))

    let grid = { Data = lines }

    grid
    |> Grid.enumerate
    |> Seq.filter (snd >> (=) (int8 0))
    |> Seq.map (fun (pos, _) ->
        (Set [ pos ], seq { (int8 1)..(int8 9) })
        ||> Seq.fold (fun set height ->
            set
            |> Seq.collect (fun node -> grid |> step node)
            |> Seq.filter (fun next -> grid[next] = height)
            |> Set)
        |> Set.count)
    |> Seq.sum
    
    |> printfn "%d"

    ()

let run2() =
    let lines =
        File.ReadAllLines("day10.txt")
        |> Array.map (String.toArray >> Array.map (fun x -> x - '0' |> int8))

    let grid = { Data = lines }

    grid
    |> Grid.enumerate
    |> Seq.filter (snd >> (=) (int8 0))
    |> Seq.map (fun (pos, _) ->
        ([ pos ], seq { (int8 1)..(int8 9) })
        ||> Seq.fold (fun set height ->
            set
            |> Seq.collect (fun node -> grid |> step node)
            |> Seq.filter (fun next -> grid[next] = height)
            |> Seq.toList)
        |> List.length)
    |> Seq.sum
    
    |> printfn "%d"

    ()
