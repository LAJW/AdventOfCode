module AdventOfCode2024.day6

// #r "nuget: FSharpPlus"
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
        ValueSome(grid[pos.Y][pos.X])
    else
        ValueNone

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

            match grid |> tryGet pos |> ValueOption.get with
            | '^'
            | '>'
            | '<'
            | 'v' -> Some(pos)
            | _ -> None))

let walk (startingPosition: Pos) (startingDirection: Pos) (visited: HashSet<struct (Pos * Pos)>) (grid: char array array) =
    let mutable direction = startingDirection
    let mutable pos = startingPosition
    visited.EnsureCapacity(5000) |> ignore // Typical path has about 4700 steps

    while grid |> tryGet pos |> ValueOption.isSome && not (visited.Contains(pos, direction)) do
        let next = pos |> add direction

        match grid |> tryGet next with
        | ValueSome('#') -> direction <- direction |> turnRight
        | _ ->
            visited.Add(pos, direction) |> ignore
            pos <- pos |> add direction

    visited, pos

let run1 () =
    let grid = File.ReadAllLines("data6.txt") |> Array.map String.toArray
    let startingPosition = getStartingPosition grid

    let startingDirection =
        grid |> tryGet startingPosition |> ValueOption.get |> arrowToDirection

    let visited = HashSet<struct (Pos * Pos)>()
    let positions =
        grid
        |> walk startingPosition startingDirection visited
        |> fst
        |> Seq.map (fun struct (a, _) -> a)
        |> Set

    printfn $"%d{positions.Count}"

let run2 () =
    let grid = File.ReadAllLines("data6.txt") |> Array.map String.toArray
    let startingPosition = getStartingPosition grid

    let startingDirection =
        grid |> tryGet startingPosition |> ValueOption.get |> arrowToDirection

    let visited = HashSet<struct (Pos * Pos)>()
    let positions =
        grid
        |> walk startingPosition startingDirection visited
        |> fst
        |> Seq.map (fun struct (a, _) -> a)
        |> Set

    let loops (grid: char array array) =
        let set, finalPos = grid |> walk startingPosition startingDirection visited
        set.Clear() // Free memory from the previous iteration
        grid |> tryGet finalPos |> ValueOption.isSome

    let loopingPositionCount =
        positions
        |> Seq.filter (fun pos ->
            let original = grid[pos.Y][pos.X]
            grid[pos.Y][pos.X] <- '#'
            let doesLoop = loops grid
            grid[pos.Y][pos.X] <- original
            doesLoop)
        |> Seq.length

    printfn $"%d{loopingPositionCount}"
