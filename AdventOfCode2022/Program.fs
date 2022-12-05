module FSharpPlayground
open FSharpPlus
open System.IO
open System

let day1_1() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.max
    |> printfn "%d"
    
let day1_2() =
    File.ReadLines("./data.txt")
    |> Seq.chunkBy(fun x -> x = "")
    |> Seq.toList
    |> Seq.filter (fun (empty, _) -> not empty)
    |> Seq.map snd
    |> Seq.map (Seq.map int >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%d"

type Shape =
    | Rock
    | Paper
    | Scissors

let day2_1() =
    let parse = function
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let round = function
        | Rock, Paper -> 0
        | Rock, Scissors -> 6
        | Paper, Rock -> 6
        | Paper, Scissors -> 0
        | Scissors, Rock -> 0
        | Scissors, Paper -> 6
        | _ -> 3
    File.ReadLines("data/day2.txt")
    |> Seq.map(String.split [" "] >> Seq.map(parse) >> Seq.toList)
    |> Seq.map(fun [theirs; mine] -> score mine + round (mine, theirs))
    |> Seq.sum
    |> printfn "%d"

type Outcome =
    | Win
    | Lose
    | Draw

let day2_2() =
    let parseRight = function
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win
    let parseLeft = function
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
    let score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
    let round = function
        | Win -> 6
        | Lose -> 0
        | Draw -> 3
    let pick = function
        | Rock, Lose -> Scissors
        | Rock, Win -> Paper
        | Paper, Lose -> Rock
        | Paper, Win -> Scissors
        | Scissors, Lose -> Paper
        | Scissors, Win -> Rock
        | shape, Draw -> shape
    File.ReadLines("data/day2.txt")
    |> Seq.map(fun pair ->
        let [| left; right |] = pair.Split(' ')
        (parseLeft left, parseRight right)
    )
    |> Seq.map(fun decision -> score (pick decision) + round (snd decision))
    |> Seq.sum
    |> printfn "%d"

module Day3 =
    let priority (ch: char) =
        if Char.IsLower ch then int ch - int 'a' + 1
        else if Char.IsUpper ch then int ch - int 'A' + 27
        else failwith $"Invalid char: {ch}"
    let part1() =
        File.ReadLines("data/day3.txt")
        |> Seq.map(fun chars ->
            let middle = chars.Length / 2
            let firstHalf = chars.Substring(0, middle) |> Set
            let secondHalf = chars.Substring(middle) |> Set
            match firstHalf |> Set.intersect secondHalf |> Set.toList with
            | [head] -> head |> priority
            | _ -> failwith "Multiple common elements"
        )
        |> Seq.sum
        |> printfn "Result: %d"

[<EntryPoint>]
let main argv =
    Day3.part1()
    0
