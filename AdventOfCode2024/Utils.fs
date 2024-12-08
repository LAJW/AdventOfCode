module AdventOfCode2024.Utils

open FSharpPlus
open System

let sfst (struct (a, _)) = a
let ssnd (struct (_, b)) = b

let isAlnum letter =
    Char.IsNumber letter || Char.IsLetter letter

type Vec =
    struct // Tuples allocate on the heap
        val X: int
        val Y: int

        new(x: int, y: int) = { X = x; Y = y }

        member this.asPair = struct (this.X, this.Y)
    end

    static member inline (+)(a: Vec, b: Vec) = Vec(a.X + b.X, a.Y + b.Y)
    static member inline (-)(a: Vec, b: Vec) = Vec(a.X - b.X, a.Y - b.Y)
    static member inline (*)(a: Vec, b: int) = Vec(a.X * b, a.Y * b)
    static member inline (*)(b: int, a: Vec) = Vec(a.X * b, a.Y * b)


type Grid<'T> =
    { Data: 'T array array }

    member this.Height = this.Data.Length
    member this.Width = this.Data[0].Length

    member this.Item
        with get (index: Vec) = this.Data[index.Y][index.X]
        and set (index: Vec) (value: 'T) = this.Data[index.Y][index.X] <- value

    member this.HasIndex(pos: Vec) =
        pos.X >= 0 && pos.Y >= 0 && pos.X < this.Width && pos.Y < this.Height

module Grid =
    let fromLines (lines: string array) =
        { Data = lines |> Array.map String.toArray }

    let indices (this: Grid<'T>) =
        Seq.allPairs (Seq.init this.Height id) (Seq.init this.Width id) |> Seq.map Vec

    let enumerate (this: Grid<'T>) =
        this |> indices |> Seq.map (fun index -> index, this[index])

    let hasIndex (pos: Vec) (this: Grid<'T>) = this.HasIndex pos

    let tryGet (pos: Vec) (this: 'T Grid) =
        if this |> hasIndex pos then
            ValueSome(this[pos])
        else
            ValueNone

    let _diagonal fn (grid: 'T Grid) =
        seq { 0 .. grid.Width + grid.Height - 2 }
        |> map (fun k ->
            seq {
                for j in 0..k do
                    let i = k - j

                    if i < grid.Height && j < grid.Width then
                        yield grid.Data[i][fn j]
            })

    let diagonalUp (grid: 'T Grid) = grid |> _diagonal id

    let diagonalDown (grid: 'T Grid) =
        grid |> _diagonal (fun j -> grid.Width - 1 - j)

    let verticalSlices (grid: 'T Grid) =
        seq { 0 .. grid.Width - 1 }
        |> map (fun x -> seq { 0 .. grid.Height - 1 } |> Seq.map (fun y -> grid.Data[y][x]))
