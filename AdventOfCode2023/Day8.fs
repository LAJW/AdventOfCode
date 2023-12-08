module Day8

open FSharpPlus
open System.IO

let data = File.ReadAllLines "./data8.txt"

let instructions = Seq.initInfinite(fun x -> data[0]) |> Seq.concat
let row = data[3]
let parseRow (row : string) =
    let [|key; value|] = row.Split(" = ")
    let [|left; right|] = value.Trim('(', ')').Split(", ")
    key, (left, right)
let graph = data |> Seq.skip 2 |> Seq.map parseRow |> Map

("AAA", instructions) ||> Seq.scan(fun state instruction ->
    let left, right = graph[state]
    match instruction with
    | 'L' -> left
    | 'R' -> right
)
|> takeWhile ((<>) "ZZZ")
|> length
|> printfn "Part 1: %d"

let startingPoints = graph |> Map.keys |> Seq.filter (fun s -> s.EndsWith('A')) |> toList

let rec gcf a b = if b = 0L then a else gcf b (a % b)
let lcm a b = (a / gcf a b) * b

// I hate that this is the solution
startingPoints |> Seq.map (fun startingPoint ->
    (startingPoint, instructions) ||> Seq.scan(fun state instruction ->
        let left, right = graph[state]
        match instruction with
        | 'L' -> left
        | 'R' -> right
    )
    |> Seq.takeWhile (fun s -> s.EndsWith('Z') |> not)
    |> Seq.length
    |> int64
)
|> Seq.reduce lcm
|> printfn "Part 2: %d"
