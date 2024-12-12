module AdventOfCode2024.day12

// #r "bin/Debug/net8.0/AdventOfCode2024.dll"
open System.IO
open System.Collections.Generic
open System.Threading
open AdventOfCode2024.Utils

let run1 () =
    let lines = File.ReadAllLines("day12.txt")
    let grid = Grid.fromLines lines
    let closed = HashSet<Vec>()

    let areas =
        grid
        |> Grid.enumerate
        |> Seq.filter (fst >> closed.Contains >> not)
        |> Seq.map (fun (pos, letter) ->
            let _open = HashSet [ pos ]
            let curClosed = HashSet<Vec> []

            while _open.Count > 0 do
                let cur = _open |> Seq.head

                Vec.Cardinals
                |> Seq.map (fun cardinal -> cardinal + cur)
                |> Seq.filter (fun next -> grid.HasIndex next && grid[next] = letter && (not <| closed.Contains next))
                |> Seq.iter (_open.Add >> ignore)

                _open.Remove(cur) |> ignore
                closed.Add(cur) |> ignore
                curClosed.Add(cur) |> ignore

            curClosed)

    areas |> Seq.map(fun area ->
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
        surfaceArea * count
    )
    |> Seq.sum |> printfn "%d"
