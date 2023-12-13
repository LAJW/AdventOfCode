#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data11.txt" |> toList

let expandRows data =
    data |> List.map (fun (row : string) ->
        if row |> Seq.forall ((<>) '#') then (Array.init row.Length (fun _ -> 'M') |> String)
        else row
    )

let width (data : string list) = data.[0].Length

let cellIndexesToExpand data = 
    seq { 0 .. (width data - 1) }
    |> filter (fun index -> data |> Seq.forall (item index >> (<>) '#'))

let expandColumns data =
    let indexes = cellIndexesToExpand data |> Set
    data |> List.map (fun row ->
        row |> enumerate |> Seq.map (fun (i, v) ->
            if indexes.Contains i then 'M' else v
        )
        |> toArray
        |> String
    )

let expanded = data |> expandColumns |> expandRows

let findPositions (data : string list) =
    data
    |> enumerate
    |> Seq.map (fun (y, v) -> y, enumerate v)
    |> Seq.collect (fun (y, row) ->
        row |> Seq.choose(fun (x, cell) ->
            if cell = '#' then Some(x, y) else None
        )
    )

let positions = expanded |> findPositions

let distance multiplier ((x1, y1), (x2, y2)) =
    let hx = max x1 x2
    let hy = max y1 y2
    let lx = min x1 x2
    let ly = min y1 y2

    let dx = seq {lx..hx} |> map (fun x -> match expanded[ly][x] with | 'M' -> multiplier | _ -> 1L) |> sum
    let dy = seq {ly..hy} |> map (fun y -> match expanded[y][hx] with | 'M' -> multiplier | _ -> 1L) |> sum

    dy + dx - 2L

let doubleResult =
    positions
    |> Seq.allPairs positions
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (distance 2L)
    |> Seq.sum
    
doubleResult / 2L |> printfn "Part 1: %d"

let doubleResult =
    positions
    |> Seq.allPairs positions
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.map (distance 1_000_000L)
    |> Seq.sum
    
doubleResult / 2L |> printfn "Part 2: %d"
