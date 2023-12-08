#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".Split("\n")

let data = File.ReadAllLines "./AdventOfCode2023/data4.txt"

data
|> map (fun row ->
    row.Split(": ").[1].Split("|")
    |> map (String.split [" "] >> filter (String.IsNullOrEmpty >> not) >> Set)
    |> Set.intersectMany
    |> Set.count
    |> (fun c -> pown 2 (c - 1))
)
|> Seq.sum
|> printfn "Part 1: %d"

let intersects =
    data
    |> map (fun row ->
        row.Split(": ").[1].Split("|")
        |> map (String.split [" "] >> filter (String.IsNullOrEmpty >> not) >> Set)
        |> Set.intersectMany
        |> Set.count
    )
    |> toList

let pairs = intersects |> mapi (fun i x -> (i + 1, x))

let rec iter (intersects : int list) : int seq =
    match intersects with
    | count :: tail ->
        seq {
            yield count
            for n in 0..count - 1 do
                yield! tail |> List.skip n |> iter
        }
    | [] -> []
    
let rec solve (intersects : int list) =
    seq {
        yield!
            match intersects with
            | [] -> Seq.empty
            | _ :: tail -> solve tail
        yield! iter intersects
    }

solve intersects |> Seq.length

let rec iter2 (intersects : (int * int) list) =
    match intersects with
    | (index, count) :: tail ->
        seq {
            yield (index, count)
            for n in 0..count - 1 do
                yield! tail |> List.skip n |> iter2
            yield! iter2 tail
        }
    | [] -> []

pairs |> skip 3 |> iter2 |> Seq.groupBy fst |> Seq.map (fun (i, c) -> i, c |> Seq.length) |> Seq.map (fun x -> x.ToString()) |> join " "
