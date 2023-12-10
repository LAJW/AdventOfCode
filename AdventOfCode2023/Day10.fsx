#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = """.....
.S-7.
.|.|.
.L-J.
.....""" |> String.split ["\n"] |> toList

let data = """..F7.
.FJ|.
SJ.L7
|F--J
LJ...""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data10.txt"

let plus (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let offsets = [(-1, 0); (0, -1); (1, 0); (0, 1)]

let tryGet (x, y) =
    if x >= 0 && y >= 0 && y < data.Length && x < data.[0].Length then Some data.[y].[x]
    else None

let neighbors point = offsets |> map (plus point)

let rec iter (closed : (int * int) Set) (open_ : (int * int) list) (distance : int) =
    // printfn "%d" distance
    // closed |> map (fun x -> x.ToString()) |> join "; " |> printfn "%s"
    // open_ |> map (fun x -> x.ToString()) |> join "; " |> printfn "%s"
    let nextOpen =
        open_
        |> Seq.bind (fun point -> offsets |> map (plus point) |> Seq.zip offsets)
        |> filter (snd >> closed.Contains >> not)
        |> filter (fun (offset, pos) ->
            match tryGet pos with
            | Some neighborValue ->
                match neighborValue, offset with
                | '|', (0, 1 | 0, -1) -> true
                | '-', (1, 0 | -1, 0) -> true
                | 'J', (1, 0 | 0, 1) -> true
                | 'F', (-1, 0 | 0, -1) -> true
                | 'L', (-1, 0 | 0, 1) -> true
                | '7', (1, 0 | 0, -1) -> true
                | _ -> false
            | None -> false
        )
        |> map snd
        |> toList
        |> distinct
    match nextOpen with
    | [] -> head open_, closed, distance
    | [ result ] -> result, closed, distance
    | nextOpen -> iter (Set.union closed (Set open_)) nextOpen (distance + 1)

let start =
    data
    |> mapi (fun i row -> (i, row))
    |> Seq.pick(fun (y, row) ->
        let x = row.IndexOf('S')
        if x >= 0 then Some (x, y) else None
    )

let open_ = [start]
let closed : (int * int) Set = Set []
let endPoint, border, distance = iter closed open_ 1
printfn "Part1: %d" distance

let rec iter2 (closed : (int * int) Set) (open_ : (int * int) list) =
    let nextOpen =
        open_
        |> Seq.bind (fun point ->
            offsets
            |> Seq.map (plus point)
            |> Seq.zip offsets
            |> filter (snd >> closed.Contains >> not)
            |> filter (snd >> tryGet >> Option.isSome)
            |> filter(fun (offset, pos) ->
                if border.Contains point then
                    match tryGet point, offset with
                    | None, _ -> failwith "Unreachable"
                    | Some('-'), (0, _) -> false
                    | Some('|'), (_, 0) -> false
                    | _ -> true
                else true
            )
        )
        |> map snd
        |> toList
        |> distinct
    match nextOpen with
    | [] -> closed
    | nextOpen -> iter2 (Set.union closed (Set open_)) nextOpen

(data[0].Length * data.Length) - (iter2 (Set []) [(0, 0)] |> Seq.length) + border.Count
