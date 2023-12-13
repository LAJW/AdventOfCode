#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data13.txt" |> toList

let patterns = data |> Seq.split [[""]] |> toList

let pattern = head patterns |> toList

let findFolds (pattern : string list) = 
    Seq.init (pattern.Length - 1) (fun index ->
        let before = pattern[0..index]
        let after = pattern[index + 1..(pattern.Length - 1)]
        index, before, after
    )
    |> Seq.filter (fun (_, before, after) -> before |> Seq.rev |> Seq.zip after |> Seq.forall(fun (a, b) -> a = b))

let rotate (pattern : string list) =
    List.init pattern[0].Length (fun x ->
        Array.init pattern.Length (fun y -> pattern[y][x]) |> String
    )

rotate ["abc"; "def"]

patterns |> Seq.map (fun pattern ->
    let pattern = pattern |> toList
    let rows = findFolds pattern |> tryHead |> Option.map (fun (index, _, _) -> index + 1) |> Option.defaultValue 0
    let columns = pattern |> rotate |> findFolds |> tryHead |> Option.map (fun (index, _, _) -> index + 1) |> Option.defaultValue 0
    rows * 100 + columns
)
|> Seq.sum
|> printfn "Part 1: %d"

let differenceIsOneCharacter (a : string) (b : string) =
    Seq.zip (toSeq a) (toSeq b) |> Seq.filter (fun (a, b) -> a <> b) |> Seq.length = 1

let findFoldsx2 (pattern : string list) = 
    Seq.init (pattern.Length - 1) (fun index ->
        let before = pattern[0..index]
        let after = pattern[index + 1..(pattern.Length - 1)]
        index, before, after
    )
    |> Seq.filter (fun (_, before, after) ->
        let zipped = before |> Seq.rev |> Seq.zip after
        let potentiallyValid = zipped |> Seq.forall(fun (a, b) -> a = b || differenceIsOneCharacter a b)
        let oneDifference = zipped |> Seq.filter(fun (a, b) -> differenceIsOneCharacter a b) |> Seq.length = 1
        potentiallyValid && oneDifference
    )

patterns |> Seq.map (fun pattern ->
    let pattern = pattern |> toList
    let rows = findFoldsx2 pattern |> tryHead |> Option.map (fun (index, _, _) -> index + 1) |> Option.defaultValue 0
    let columns = pattern |> rotate |> findFoldsx2 |> tryHead |> Option.map (fun (index, _, _) -> index + 1) |> Option.defaultValue 0
    rows * 100 + columns
)
|> Seq.sum
