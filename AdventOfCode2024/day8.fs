module AdventOfCode2024.day8

open System
open System.IO
open FSharpPlus
open AdventOfCode2024.Utils.Pos


let contains (pos: Pos) (grid: string array) =
    (pos.X >= 0 && pos.Y >= 0 && pos.X < grid[0].Length && pos.Y < grid.Length)

let isAlnum letter =
    Char.IsNumber letter || Char.IsLetter letter

let run1 () =
    let lines = File.ReadAllLines("data8.txt")

    let map =
        Seq.allPairs (Seq.init lines[0].Length id) (Seq.init lines.Length id)
        |> Seq.map Pos
        |> Seq.choose (fun pos ->
            let node = lines[pos.X][pos.Y]

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
        |> Seq.collect (fun (a, b) ->
            let diff = b - a
            [ (a - diff); (b + diff) ])
        |> Seq.filter (fun pos -> pos.X >= 0 && pos.Y >= 0 && pos.X < lines[0].Length && pos.Y < lines.Length)
        |> Set

    // for y in 0..lines.Length - 1 do
    //     for x in 0..lines[0].Length - 1 do
    //         let pos = Pos(y, x)
    //         let ch = lines[y][x]
    //         if antinodes.Contains pos && not <| isAlnum ch then
    //             printf "#"
    //         else printf "%c" ch
    //     printfn ""

    antinodes.Count |> printfn "%d"

let run2 () =
    let lines = File.ReadAllLines("data8.txt")

    let map =
        Seq.allPairs (Seq.init lines[0].Length id) (Seq.init lines.Length id)
        |> Seq.map Pos
        |> Seq.choose (fun pos ->
            let node = lines[pos.X][pos.Y]

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
        |> Seq.collect (fun (a, b) ->
            let diff = b - a
            let before = Seq.initInfinite (fun i -> a - diff * i) |> Seq.takeWhile (fun pos -> lines |> contains pos)
            let after = Seq.initInfinite (fun i -> b + diff * i) |> Seq.takeWhile (fun pos -> lines |> contains pos)
            Seq.append before after)
        |> Seq.filter (fun pos -> pos.X >= 0 && pos.Y >= 0 && pos.X < lines[0].Length && pos.Y < lines.Length)
        |> Set

    antinodes.Count |> printfn "%d"
