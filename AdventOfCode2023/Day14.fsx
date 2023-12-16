#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data14.txt" |> toList

let rotate (pattern : string list) =
    List.init pattern[0].Length (fun x ->
        Array.init pattern.Length (fun y -> pattern[y][x]) |> String
    )

let load (row : string) =
    row
    |> enumerate
    |> Seq.chunkBy (fun (i, x) -> x <> '#')
    |> Seq.filter (fst >> (=) true)
    |> Seq.map (snd >> toList >> function
        | head :: rest ->
            let length = head :: rest |> Seq.filter (snd >> (=) 'O') |> Seq.length
            seq { 0 .. (length - 1) } |> Seq.map (fun i -> row.Length - (fst head + i)) |> Seq.sum
        | _ -> 0
    )
    |> Seq.sum
    
rotate data |> map load |> Seq.sum |> printfn "Part1: %d"

let total_iterations = 1_000_000_000

let roll (row : string) =
    row
    |> Seq.chunkBy ((<>) '#')
    |> Seq.map snd
    |> Seq.map Seq.sortDescending
    |> Seq.concat
    |> toArray
    |> String

let rotateClockwise (pattern : string list) =
    List.init pattern[0].Length (fun x ->
        Array.init pattern.Length (fun y -> pattern[pattern.Length - y - 1][x]) |> String
    )
    
let before data = data |> rotateClockwise |> rotateClockwise|> rotateClockwise
let after = rotateClockwise

let cycle data =
    let result =
        data
        |> map roll |> rotateClockwise
        |> map roll |> rotateClockwise
        |> map roll |> rotateClockwise
        |> map roll |> rotateClockwise
    result

let getLoad (data : string list) =
    data |> rotate |> Seq.map (enumerate >> Seq.filter (snd >> (=) 'O') >> Seq.map (fst >> fun x -> data.Length - x) >> Seq.sum)  |> Seq.sum

let repeat count func (data : string list) =
    let memory = Dictionary<string list, string list * int>()
    let mutable i = 0
    let mutable data = data
    while i < count do
        data <-
            match memory.TryGetValue data with
            | true, (value, index) ->
                let loop_length = i - index
                while i + loop_length < count do
                    i <- i + loop_length
                value
            | false, _ ->
                let result = func data
                memory[data] <- (result, i)
                result
        i <- i + 1
    data

data
|> before
|> repeat 1000000000 cycle
|> after
|> getLoad
|> printfn "Part 2: %d"
