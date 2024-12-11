module AdventOfCode2024.day11

open System.IO
open System.Numerics
open AdventOfCode2024.Utils

let stripZeroes (str: string) = str |> BigInteger.Parse |> string

let solve (n : int) =
    let rec iter =
        memoize (fun (stone, n) ->
            if n = 0 then
                1L
            else if stone = "0" then
                iter ("1", n - 1)
            elif stone.Length % 2 = 0 then
                let middle = stone.Length / 2 - 1
                let left = iter (stone[0..middle], n - 1)
                let right = iter (stone[middle + 1 ..] |> stripZeroes, n - 1)
                left + right
            else
                iter (BigInteger.Parse(stone) * BigInteger(2024) |> string, n - 1))

    let lines = File.ReadAllLines("day11.txt")
    let stones = lines[0].Split(" ")

    stones |> Seq.map (fun stone -> iter (stone, n)) |> Seq.sum |> printfn "%d"

let run1() = solve 25
let run2() = solve 75
