module Day3
open System
open Extensions

type MutableList<'u> = System.Collections.Generic.List<'u>

let createGrid size =
    Array2D.init size size (fun _ _ -> false)

let parseSegment (str : string) =
    let direction = str |> Seq.head
    let length = str |> Seq.tail |> Seq.map string |> String.join "" |> int
    match direction with
    | 'L' -> -1, 0, length
    | 'R' -> 1, 0, length
    | 'U' -> 0, 1, length
    | 'D' -> 0, -1, length

let parseWire = String.split ',' >> Seq.map parseSegment

let nextPos (x, y) (dirX, dirY, length) =
    x + dirX * length, y + dirY * length

let rec wireToSegments (pos : int * int) = function
    | head::tail ->
        let newPos = nextPos pos head
        (pos, newPos) :: wireToSegments newPos tail
    | [] -> []

let orient segment =
    let (x1, y1), (x2, y2) = segment
    (min x1 x2, min y1 y2), (max x1 x2, max y1 y2)

let intersect segment1 segment2 =
    // Not my proudest accomplishment
    let (s1x1, s1y1), (s1x2, s1y2) = orient segment1
    let (s2x1, s2y1), (s2x2, s2y2) = orient segment2
    if s1x1 = s1x2 && s2y1 = s2y2 then
        let x = s1x1
        let y = s2y1
        if x >= s2x1 + 1 && x <= s2x2 - 1 && y >= s1y1 + 1 && y <= s1y2 - 1 then
            Some (x, y)
        else None
    else if s1y1 = s1y2 && s2x1 = s2x2 then
        let x = s2x1
        let y = s1y1
        if x >= s1x1 + 1 && x <= s1x2 - 1 && y >= s2y1 + 1 && y <= s2y2 - 1 then
            Some (x, y)
        else None
    else None

// Count needs to be passed in for the tail call recursion to work - adding one
// afterwards doesn't cut it
let rec countSteps pos dest count = function
    | head :: tail ->
        let dirX, dirY, length = head
        let newPos = nextPos pos (dirX, dirY, 1)
        if newPos = dest then
            Some (count + 1)
        else if length > 1 then
            (dirX, dirY, length - 1) :: tail |> countSteps newPos dest (count + 1)
        else tail |> countSteps newPos dest (count + 1)
    | [] -> None

let intersectionSteps (rawWires : string list) =
    let wires = rawWires |> List.map (parseWire >> Seq.toList)
    let segments = wires |> List.collect(wireToSegments (0, 0))
    Seq.allPairs segments segments
    |> Seq.choose (fun (s1, s2) -> intersect s1 s2)
    |> Seq.choose(fun intersection ->
        wires
        |> Seq.choose (countSteps (0, 0) intersection 0)
        |> Seq.sort
        |> Seq.tryTake 2
        |> Option.map Seq.sum)
    |> Seq.min

let distance (wires : string list) =
    let segments = wires |> List.collect (parseWire >> Seq.toList >> wireToSegments (0, 0))
    Seq.allPairs segments segments
    |> Seq.choose (fun (s1, s2) -> intersect s1 s2)
    |> Seq.map (fun (x, y) -> abs(x) + abs(y))
    |> Seq.min
