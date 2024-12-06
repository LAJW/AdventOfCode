namespace AdventOfCode2024.Utils

open FSharpPlus

type Grid(text: string array) =
    let height = text.Length
    let width = text[0].Length

    let diagonal fn =
        seq { 0 .. width + height - 2 }
        |> map (fun k ->
            seq {
                for j in 0..k do
                    let i = k - j

                    if i < height && j < width then
                        yield text[i][fn j]
            }
            |> String.ofSeq)

    member _.Vertical =
        seq { 0 .. width - 1 }
        |> map (fun x -> seq { 0 .. height - 1 } |> Seq.map (fun y -> text[y][x]) |> String.ofSeq)

    member _.DiagonalUp = diagonal id

    member _.DiagonalDown = diagonal (fun j -> width - 1 - j)
