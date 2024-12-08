module AdventOfCode2024.day6

// #r "nuget: FSharpPlus"
open System.Collections.Generic
open System.IO
open FSharpPlus
open AdventOfCode2024.Utils

let turnRight (dir: Vec) =
    match dir.asPair with
    | 1, 0 -> Vec(0, 1)
    | 0, 1 -> Vec(-1, 0)
    | -1, 0 -> Vec(0, -1)
    | 0, -1 -> Vec(1, 0)
    | _ -> failwith "function only works with directional vectors"

let tryGet (pos: Vec) (grid: char array array) =
    if pos.X >= 0 && pos.Y >= 0 && pos.X < grid[0].Length && pos.Y < grid.Length then
        ValueSome(grid[pos.Y][pos.X])
    else
        ValueNone

let arrowToDirection ch =
    match ch with
    | '^' -> Vec(0, -1)
    | '>' -> Vec(0, 1)
    | '<' -> Vec(-1, 0)
    | 'v' -> Vec(0, 1)
    | _ -> failwith "bad starting position"

let getStartingPosition (grid: char array array) =
    seq { 0 .. grid.Length - 1 }
    |> Seq.pick (fun y ->
        seq { 0 .. grid[y].Length - 1 }
        |> Seq.tryPick (fun x ->
            let pos = Vec(x, y)

            match grid |> tryGet pos |> ValueOption.get with
            | '^'
            | '>'
            | '<'
            | 'v' -> Some(pos)
            | _ -> None))

let walk (startingPosition: Vec) (startingDirection: Vec) (visited: HashSet<struct (Vec * Vec)>) (grid: char array array) =
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

    let visited = HashSet<struct (Vec * Vec)>()
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

    let visited = HashSet<struct (Vec * Vec)>()
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
