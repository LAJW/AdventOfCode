module AdventOfCode2024.Utils

open FSharpPlus

type Grid1(text: string array) =
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

type Vec =
    struct // Tuples allocate on the heap
        val X: int
        val Y: int

        new(x: int, y: int) = { X = x; Y = y }

        member this.asPair = struct (this.X, this.Y)
    end

    static member (+)(a: Vec, b: Vec) = Vec(a.X + b.X, a.Y + b.Y)
    static member (-)(a: Vec, b: Vec) = Vec(a.X - b.X, a.Y - b.Y)
    static member (*)(a: Vec, b: int) = Vec(a.X * b, a.Y * b)
    static member (*)(b: int, a: Vec) = Vec(a.X * b, a.Y * b)

let sfst(struct(a, _)) = a
let ssnd(struct(_, b)) = b

type Grid<'T> =
    { Data: 'T array array }

    member this.Height = this.Data.Length
    member this.Width = this.Data[0].Length
    member this.Item
        with get(index: Vec) = this.Data[index.Y][index.X]
        and set(index: Vec) (value: 'T) =
            this.Data[index.Y][index.X] <- value

module Grid =
    let fromLines (lines: string array) =
        { Data = lines |> Array.map String.toArray }

    let indices (this: Grid<'T>) =
        Seq.allPairs (Seq.init this.Height id) (Seq.init this.Width id) |> Seq.map Vec

    let enumerate (this: Grid<'T>) =
        this |> indices |> Seq.map (fun index -> index, this[index])

    let hasIndex (pos: Vec) (this: Grid<'T>) =
        pos.X >= 0 && pos.Y >= 0 && pos.X < this.Width && pos.Y < this.Height

    let tryGet (pos: Vec) (this: char Grid) =
        if this |> hasIndex pos then
            ValueSome(this[pos])
        else
            ValueNone
