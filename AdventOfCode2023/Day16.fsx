#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....""" |> String.split ["\n"] |> toList

let data = File.ReadLines "./AdventOfCode2023/data16.txt" |> toList

// naive
// it's possible it will hit a loop. But the only way to loop is to hit a splitter.

let add (xa, ya) (xb, yb) = (xa + xb, ya + yb)

let configurations =
    seq {
        for y in 0..data.Length - 1 do
            yield (0, y), (1, 0)
            yield (data[0].Length - 1, y), (-1, 0)
        for x in 0..data[0].Length - 1 do
            yield (x, 0), (0, 1)
            yield (x, data.Length - 1), (0, -1)
    }

let solve configurations =
    configurations |> Seq.map (fun (point, direction) ->
        let seen = HashSet<int * int>()
        let alreadySplit = HashSet<int * int>()

        let rec trace point direction =
            let inline turn direction = trace (add direction point) direction
            let x, y = point
            if x >= 0 && y >= 0 && y < data.Length && x < data[0].Length && not (alreadySplit.Contains point) then
                seen.Add point |> ignore
                match direction, data.[snd point].[fst point] with
                | _, '.' | (_, 0), '-' | (0, _), '|' -> trace (add direction point) direction
                | (1, 0), '/' -> turn (0, -1)
                | (0, 1), '/' -> turn (-1, 0)
                | (-1, 0), '/' -> turn (0, 1)
                | (0, -1), '/' -> turn (1, 0)
                | (1, 0), '\\' -> turn (0, 1)
                | (0, 1), '\\' -> turn (1, 0)
                | (-1, 0), '\\' -> turn (0, -1)
                | (0, -1), '\\' -> turn (-1, 0)
                | (_, 0), '|' ->
                    alreadySplit.Add point |> ignore
                    turn (0, -1)
                    turn (0, 1)
                | (0, _), '-' ->
                    alreadySplit.Add point |> ignore
                    turn (-1, 0)
                    turn (1, 0)
                | a, b -> printfn $"Bad match {a} {b}"
            else
                ()

        trace point direction

        seen.Count
    )
    |> Seq.max

solve [(0, 0), (1, 0)] |> printfn "Part 1: %d"
solve configurations |> printfn "Part 2: %d"
