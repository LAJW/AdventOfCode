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

let countEdges line (area: IReadOnlySet<Vec>) =
    line
    |> Seq.map area.Contains
    |> Seq.pairwise
    |> Seq.countIf (fun (a, b) -> a <> b)

let cost (area: IReadOnlySet<Vec>) =
    let surfaceArea = area.Count
    let lo, hi = findMinMax area

    let count =
        (lo - Vec(1, 1) |> Vec.untilVertically (hi + Vec(2, 2)))
        |> Seq.append (lo - Vec(1, 1) |> Vec.untilHorizontally (hi + Vec(2, 2)))
        |> Seq.map (fun line -> area |> countEdges line)
        |> Seq.sum

    surfaceArea * count

let discountedCost (area: IReadOnlySet<Vec>) =
    let surfaceArea = area.Count
    let lo, hi = findMinMax area

    let verticalStripes =
        (lo - Vec(1, 1) |> Vec.untilVertically (hi + Vec(2, 2)))
        |> Seq.map (Seq.map (fun pos -> area.Contains pos, area.Contains(pos + Vec(1, 0))))

    let horizontalStripes =
        (lo - Vec(1, 1) |> Vec.untilHorizontally (hi + Vec(2, 2)))
        |> Seq.map (Seq.map (fun pos -> area.Contains pos, area.Contains(pos + Vec(0, 1))))

    let count =
        Seq.append verticalStripes horizontalStripes
        |> Seq.map (
            Seq.pairwise
            >> Seq.countIf (fun (prev, cur) ->
                let a, b = cur
                a <> b && cur <> prev)
        )
        |> Seq.sum

    surfaceArea * count

let solve costFunction =
    benchmark (fun () ->
        File.ReadAllLines("day12.txt")
        |> Grid.fromLines
        |> findAreas
        |> Seq.map costFunction
        |> Seq.sum
        |> printfn "%d")
    

let run1 () = solve cost

let run2 () = solve discountedCost
