module AdventOfCode2024.day6

// #r "nuget: FSharpPlus"
open System
open System.Collections.Generic
open System.IO
open FSharpPlus

// Tuples allocate on the heap
type Pos =
    struct
        val X: int
        val Y: int

        new(x: int, y: int) = { X = x; Y = y }

        member this.asPair = struct (this.X, this.Y)
    end

let turnRight (dir: Pos) =
    match dir.asPair with
    | 1, 0 -> Pos(0, 1)
    | 0, 1 -> Pos(-1, 0)
    | -1, 0 -> Pos(0, -1)
    | 0, -1 -> Pos(1, 0)
    | _ -> failwith "function only works with directional vectors"

let add (a: Pos) (b: Pos) = Pos(a.X + b.X, a.Y + b.Y)

let tryGet (pos: Pos) (grid: char array array) =
    if pos.X >= 0 && pos.Y >= 0 && pos.X < grid[0].Length && pos.Y < grid.Length then
        Some(grid[pos.Y][pos.X])
    else
        None

let arrowToDirection ch =
    match ch with
    | '^' -> Pos(0, -1)
    | '>' -> Pos(0, 1)
    | '<' -> Pos(-1, 0)
    | 'v' -> Pos(0, 1)
    | _ -> failwith "bad starting position"

let getStartingPosition (grid: char array array) =
    seq { 0 .. grid.Length - 1 }
    |> Seq.pick (fun y ->
        seq { 0 .. grid[y].Length - 1 }
        |> Seq.tryPick (fun x ->
            let pos = Pos(x, y)

            match grid |> tryGet pos |> Option.get with
            | '^'
            | '>'
            | '<'
            | 'v' -> Some(pos)
            | _ -> None))

let walk (startingPosition: Pos) (startingDirection: Pos) (grid: char array array) =
    let mutable direction = startingDirection
    let mutable pos = startingPosition
    let visited = HashSet<struct (Pos * Pos)>()

    while grid |> tryGet pos |> Option.isSome && not (visited.Contains(pos, direction)) do
        let next = pos |> add direction

        match grid |> tryGet next with
        | Some('#') -> direction <- direction |> turnRight
        | _ ->
            visited.Add(pos, direction) |> ignore
            pos <- pos |> add direction

    visited, pos

let run1 () =
    let grid = File.ReadAllLines("data6.txt") |> Array.map String.toArray
    let startingPosition = getStartingPosition grid

    let startingDirection =
        grid |> tryGet startingPosition |> Option.get |> arrowToDirection

    let positions =
        grid
        |> walk startingPosition startingDirection
        |> fst
        |> Seq.map (fun struct (a, b) -> a)
        |> Set

    printfn "%d" positions.Count

let run2 () =
    let grid = File.ReadAllLines("data6.txt") |> Array.map String.toArray
    let startingPosition = getStartingPosition grid

    let startingDirection =
        grid |> tryGet startingPosition |> Option.get |> arrowToDirection

    let positions =
        grid
        |> walk startingPosition startingDirection
        |> fst
        |> Seq.map (fun struct (a, b) -> a)
        |> Set

    let loops (grid: char array array) =
        let finalPos = grid |> walk startingPosition startingDirection |> snd
        grid |> tryGet finalPos |> Option.isSome

    let loopingPositionCount =
        positions
        |> Seq.filter (fun pos ->
            let original = grid[pos.Y][pos.X]
            grid[pos.Y][pos.X] <- '#'
            let doesLoop = loops grid
            grid[pos.Y][pos.X] <- original
            doesLoop)
        |> Seq.length

    printfn "%d" loopingPositionCount
