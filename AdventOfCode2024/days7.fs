module AdventOfCode2024.daya7

open System.IO
open FSharpPlus

let concat (a : int64) (b: int64) = int64(string a + string b)

let rec canDo (ops : (int64 -> int64 -> int64) list) (expected : int64) (numbers : int64 list) =
    match numbers with
    | [] -> false
    | [value] -> expected = value
    | a :: b :: rest -> ops |> List.exists (fun op -> canDo ops expected (op a b :: rest))

let solve canDo =
    File.ReadAllLines("data7.txt")
    |> Seq.map (fun line ->
        let [| controlStr; numbersStr |] = line.Split(": ")
        let control = int64 controlStr
        let numbers = numbersStr.Split(" ") |> Seq.map int64 |> toList

        if canDo control numbers then control else 0 
    )
    |> Seq.sum
    |> printfn "%d"

let run1() = solve (canDo [(+); (*)])
let run2() = solve (canDo [(+); (*); concat])
