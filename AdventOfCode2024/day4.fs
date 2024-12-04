module AdventOfCode2024.day4

open System.Text.RegularExpressions
open System.IO
open FSharpPlus

let run1 () =
    let text = File.ReadAllLines "./data4.txt"
    let countXmas line =
       Regex.Matches(line, "XMAS").Count + Regex.Matches(rev line, "XMAS").Count
    let horizontal = text |> map countXmas |> Seq.sum
    
    let dim = text[0].Length
    
    let mutable verticalCount = 0
    for x in 0..dim - 1 do
        let mutable str = ""
        for y in 0..dim - 1 do
            str <- str + string(text[y][x])
        printfn "%s" str
        verticalCount <- verticalCount + countXmas str
    
    let mutable count = 0
    for k in 0 .. dim * 2 do
        let mutable str = ""
        for j in 0 .. k do
            let i = k - j
            if i < dim && j < dim then
                str <- str + string(text[i][j])
        printfn "%s" str
        count <- count + countXmas str

    for k in 0 .. dim * 2 do
        let mutable str = ""
        for j in 0 .. k do
            let i = k - j
            if i < dim && j < dim then
                str <- str + string(text[i][dim - 1 - j])
        printfn "%s" str
        count <- count + countXmas str
        
    (verticalCount + count + horizontal) |> printfn "%d"


let run2 () =
    let text = File.ReadAllLines "./data4.txt"

    let dim = text.Length
    let indexes = seq { 1 .. dim - 2 }

    Seq.allPairs indexes indexes
    |> Seq.filter (fun (x, y) ->
        let s1 = String.ofList [ text[y - 1][x - 1]; text[y][x]; text[y + 1][x + 1] ]
        let s2 = String.ofList [ text[y + 1][x - 1]; text[y][x]; text[y - 1][x + 1] ]
        (s1 = "MAS" || s1 = "SAM") && (s2 = "MAS" || s2 = "SAM"))
    |> Seq.length
    |> printfn "%d"
