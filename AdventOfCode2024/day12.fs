module AdventOfCode2024.day12

open System.IO
open System.Collections.Generic
open AdventOfCode2024.Utils

let findMinMax (set: Vec seq) =
    let minX = set |> Seq.map (_.X) |> Seq.min
    let minY = set |> Seq.map (_.Y) |> Seq.min
    let maxX = set |> Seq.map (_.X) |> Seq.max
    let maxY = set |> Seq.map (_.Y) |> Seq.max
    Vec(minX, minY), Vec(maxX, maxY)

let findAreas (grid: Grid<_>) =
    let closed = HashSet<Vec>()

    grid
    |> Grid.enumerate
    |> Seq.filter (fst >> closed.Contains >> not)
    |> Seq.map (fun (pos, _) ->
        let area = grid |> Grid.floodFill pos
        closed.UnionWith(area)
        area)

let run1 () =
    let lines = File.ReadAllLines("day12.txt")
    let grid = Grid.fromLines lines

    let areas = grid |> findAreas

    areas
    |> Seq.map (fun area ->
        let surfaceArea = area.Count

        let countVertical =
            grid
            |> Grid.verticalIndexes
            |> Seq.map (fun line ->
                let tmp = line |> Seq.map area.Contains |> Seq.toList

                [ false ] @ tmp @ [ false ]
                |> Seq.pairwise
                |> Seq.filter (fun (a, b) -> a <> b)
                |> Seq.length)
            |> Seq.sum

        let countHorizontal =
            grid
            |> Grid.horizontalIndexes
            |> Seq.map (fun line ->
                let tmp = line |> Seq.map area.Contains |> Seq.toList

                [ false ] @ tmp @ [ false ]
                |> Seq.pairwise
                |> Seq.filter (fun (a, b) -> a <> b)
                |> Seq.length)
            |> Seq.sum

        let count = countHorizontal + countVertical
        surfaceArea * count)
    |> Seq.sum
    |> printfn "%d"

let run2 () =
    benchmark (fun () ->
        File.ReadAllLines("day12.txt")
        |> Grid.fromLines
        |> findAreas
        |> Seq.map (fun area ->
            let surfaceArea = area.Count
            let lo, hi = findMinMax area
            let mutable count = 0

            for x in lo.X - 1 .. hi.X do
                let mutable prev = struct (false, false)

                for y in lo.Y - 1 .. hi.Y do
                    let a, b = area.Contains(Vec(x, y)), area.Contains(Vec(x + 1, y))
                    let cur = struct (a, b)

                    if a <> b && cur <> prev then
                        count <- count + 1

                    prev <- cur

            for y in lo.Y - 1 .. hi.Y do
                let mutable prev = struct (false, false)

                for x in lo.X - 1 .. hi.X do
                    let a, b = area.Contains(Vec(x, y)), area.Contains(Vec(x, y + 1))
                    let cur = struct (a, b)

                    if a <> b && cur <> prev then
                        count <- count + 1

                    prev <- cur

            surfaceArea * count)
        |> Seq.sum
        |> printfn "%d")
