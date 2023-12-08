#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open System.Buffers.Text
open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)""".Split("\n")

let data = """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)""".Split("\n")

let data = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)""".Split("\n")

let data = File.ReadAllLines "./AdventOfCode2023/data8.txt"

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
|> Seq.takeWhile ((<>) "ZZZ")
|> Seq.length
|> printfn "Part 1: %d"

let startingPoints = graph |> Map.keys |> Seq.filter (fun s -> s.EndsWith('A')) |> toList

let rec gcf a b = if b = 0L then a else gcf b (a % b)
let lcm a b = (a / gcf a b) * b

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
|> Seq.toList
|> Seq.reduce lcm
|> printfn "Part 2: %d"
