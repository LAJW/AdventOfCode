module AdventOfCode2024.day8

open System
open System.IO
open FSharpPlus
open AdventOfCode2024.Utils

let solve (getAntinodes: Vec -> Vec -> char Grid -> Vec seq) =
    let grid = File.ReadAllLines("data8.txt") |> Grid.fromLines

    let map =
        grid
        |> Grid.enumerate
        |> Seq.choose (fun (pos, node) ->
            if Char.IsNumber(node) || Char.IsLetter(node) then
                Some(pos, node)
            else
                None)
        |> Seq.groupBy snd
        |> Seq.map (fun (key, nodes) -> key, (nodes |> Seq.map fst |> toList))
        |> Map

    let antinodes =
        map
        |> Map.values
        |> Seq.collect (fun positions -> positions |> Seq.allPairs positions)
        |> Seq.filter (fun (a, b) -> a <> b)
        |> Seq.collect (fun (a, b) -> getAntinodes a b grid)
        |> Seq.filter (fun pos -> grid |> Grid.hasIndex pos)
        |> Set

    antinodes.Count |> printfn "%d"

let run1 () =
    solve (fun a b _ ->
        let diff = b - a
        [ (a - diff); (b + diff) ])

let run2 () =
    solve (fun a b grid ->
        let diff = b - a

        let before =
            Seq.initInfinite (fun i -> a - diff * i)
            |> Seq.takeWhile (fun pos -> grid |> Grid.hasIndex pos)

        let after =
            Seq.initInfinite (fun i -> b + diff * i)
            |> Seq.takeWhile (fun pos -> grid |> Grid.hasIndex pos)

        Seq.append before after)
