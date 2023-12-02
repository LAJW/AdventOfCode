module AdventOfCode2023
open System
open System.IO
open System.Text.RegularExpressions
open FSharpPlus

let onlyDigits s = s |> Seq.filter Char.IsNumber
let firstAndLast seq = Seq.head seq, Seq.last seq
let regex = Regex "(?=(one|two|three|four|five|six|seven|eight|nine|\\d))"

let parse str : int seq =
    monad' {
        let! match_ = regex.Matches str
        let! group = match_.Groups.Values
        let value = group.Value
        return
            match value with
            | "" -> None
            | "0" | "zero" -> Some 0
            | "1" | "one" -> Some 1
            | "2" | "two" -> Some 2
            | "3" | "three" -> Some 3
            | "4" | "four" -> Some 4
            | "5" | "five" -> Some 5
            | "6" | "six" -> Some 6
            | "7" | "seven" -> Some 7
            | "8" | "eight" -> Some 8
            | "9" | "nine" -> Some 9
            | x -> failwith ("unknown input \"\"" + x)
    }
    |> Seq.choose id

let concatPair(a, b) = $"{a}{b}"

let rows = File.ReadAllLines "./data1.txt"

let solve parse = rows |> Seq.map (parse >> firstAndLast >> concatPair >> Int32.Parse) |> Seq.sum

solve onlyDigits |> printfn "Part 1: %d"    
solve parse |> printfn "Part2: %d"
