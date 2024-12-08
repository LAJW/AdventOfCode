module AdventOfCode2024.day6

open System.Collections.Generic
open System.IO
open AdventOfCode2024.Utils

let turnRight (dir: Vec) =
    match dir.asPair with
    | 1, 0 -> Vec(0, 1)
    | 0, 1 -> Vec(-1, 0)
    | -1, 0 -> Vec(0, -1)
    | 0, -1 -> Vec(1, 0)
    | _ -> failwith "function only works with directional vectors"

let arrowToDirection ch =
    match ch with
    | '^' -> Vec(0, -1)
    | '>' -> Vec(0, 1)
    | '<' -> Vec(-1, 0)
    | 'v' -> Vec(0, 1)
    | _ -> failwith "bad starting position"

let getStartingPosition (grid: char Grid) =
    grid
    |> Grid.enumerate
    |> Seq.pick (fun (pos, ch) ->
        match ch with
        | '^'
        | '>'
        | '<'
        | 'v' -> Some(pos)
        | _ -> None)

let walk (startingPosition: Vec) (startingDirection: Vec) (visited: HashSet<struct (Vec * Vec)>) (grid: char Grid) =
    let mutable direction = startingDirection
    let mutable pos = startingPosition
    visited.EnsureCapacity(5000) |> ignore // Typical path has about 4700 steps

    while grid |> Grid.hasIndex pos && not <| visited.Contains(pos, direction) do
        let next = pos + direction

        match grid |> Grid.tryGet next with
        | ValueSome('#') -> direction <- direction |> turnRight
        | _ ->
            visited.Add(pos, direction) |> ignore
            pos <- pos + direction

    visited, pos

let run1 () =
    let grid = File.ReadAllLines("data6.txt") |> Grid.fromLines
    let startingPosition = getStartingPosition grid
    let startingDirection = grid[startingPosition] |> arrowToDirection
    let visited = HashSet<struct (Vec * Vec)>()

    let positions =
        grid
        |> walk startingPosition startingDirection visited
        |> fst
        |> Seq.map sfst
        |> Set

    printfn $"%d{positions.Count}"

let run2 () =
    let grid = File.ReadAllLines("data6.txt") |> Grid.fromLines
    let startingPosition = getStartingPosition grid
    let startingDirection = grid[startingPosition] |> arrowToDirection
    let visited = HashSet<struct (Vec * Vec)>()

    let positions =
        grid
        |> walk startingPosition startingDirection visited
        |> fst
        |> Seq.map sfst
        |> Set

    let loops (grid: char Grid) =
        let set, finalPos = grid |> walk startingPosition startingDirection visited
        set.Clear() // Free memory from the previous iteration
        grid |> Grid.hasIndex finalPos

    let loopingPositionCount =
        positions
        |> Seq.filter (fun pos ->
            let original = grid[pos]
            grid[pos] <- '#'
            let doesLoop = loops grid
            grid[pos] <- original
            doesLoop)
        |> Seq.length

    printfn $"%d{loopingPositionCount}"
