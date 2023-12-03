module Day3

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

// let data = """467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..""".Split("\n")

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let data = File.ReadAllLines "./data3.txt"

let tryGet x y =
    if x >= 0 && y >= 0 && y < data.Length && x < data[y].Length then
        Some (data[y][x])
    else None
    
let isSymbol ch = "!@#$%^&*()+=-/|" |> Seq.contains ch

let isPartDigit x y =
    let current = data[y][x]
    if Char.IsDigit current then
        let offset = [ -1; 0; 1 ]
        Seq.allPairs offset offset
        |> map (fun (ox, oy) -> x + ox, y + oy)
        |> Seq.filter ((<>) (x, y))
        |> choose (fun (x, y) -> tryGet x y)
        |> Seq.exists isSymbol
    else
        false

let isPartNumber y (match_ : Match) =
    let sx = match_.Index
    let ex = match_.Index + match_.Length
    seq { sx .. ex - 1 } |> exists (fun x -> isPartDigit x y)

let regex = Regex("\d+")

data
|> Seq.mapi (fun y row -> regex.Matches row |> Seq.filter (isPartNumber y))
|> Seq.concat
|> map (value >> Int32.Parse)
|> sum
|> printfn "Part 1: %d"
    
let partNumberMatches =
    data
    |> Seq.mapi (fun y row -> regex.Matches row |> Seq.filter (isPartNumber y) |> map (fun m -> y, m))
    |> Seq.concat
    |> Seq.toList

let gearRegex = Regex("\*")

data |> Seq.mapi (fun y row -> gearRegex.Matches row |> Seq.map (fun x -> x.Index, y))
|> Seq.concat
|> map (fun (x, y) ->
    partNumberMatches
    |> Seq.filter (fun (my, m : Match) -> Math.Abs(my - y) <= 1 && m.Index - 1 <= x && x <= m.Index + m.Length)
    |> map (snd >> value >> Int64.Parse)
    |> toList
)
|> filter (length >> (=) 2)
|> map (List.reduce (*))
|> sum
|> printfn "Part 2: %d"
