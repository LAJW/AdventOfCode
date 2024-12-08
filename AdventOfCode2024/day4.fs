module AdventOfCode2024.day4

open System.Text.RegularExpressions
open System.IO
open FSharpPlus
open AdventOfCode2024.Utils

let run1 () =
    let text = File.ReadAllLines "./data4.txt"

    let countXmas line =
        Regex.Matches(line, "XMAS").Count + Regex.Matches(rev line, "XMAS").Count

    let grid = Grid1 text

    [ toSeq text; grid.Vertical; grid.DiagonalUp; grid.DiagonalDown ]
    |> Seq.concat
    |> map countXmas
    |> Seq.sum
    |> printfn "%d"


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
