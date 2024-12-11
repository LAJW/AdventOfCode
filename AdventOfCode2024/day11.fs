module AdventOfCode2024.day11

open System
open System.IO
open System.Numerics
open System.Collections.Generic

let stripZeroes (str: string) = str |> BigInteger.Parse |> string

let rec iter (m: Dictionary<int * string, int64>) (n : int) (stone: string) =
    match m.TryGetValue ((n, stone)) with
    | true, value -> value
    | false, _ ->
        let result =
            if n = 0 then
                1L
            else
                if stone = "0" then
                    "1" |> iter m (n - 1)
                elif stone.Length % 2 = 0 then
                    let middle = stone.Length / 2 - 1
                    let left = stone[0..middle] |> iter m (n - 1)
                    let right = stone[middle + 1 .. ] |> stripZeroes |> iter m (n - 1)
                    left + right
                else
                    BigInteger.Parse(stone) * BigInteger(2024) |> string |> iter m (n - 1)
        m.Add((n, stone), result)
        printfn "%d" m.Count
        result

[<EntryPoint>]
let main (argv: string array) =
    let lines = File.ReadAllLines("day11.txt")
    let stones = lines[0].Split(" ")
    let m = Dictionary<int * string, int64>()
    
    stones |> Seq.map (iter m 75) |> Seq.sum |> printfn "%d"
    
    0
