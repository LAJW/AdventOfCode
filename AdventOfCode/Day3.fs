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

let isHorizontal ((x1, _y1), (x2, _y2)) = x1 = x2

let isVertical ((_x1, y1), (_x2, y2)) = y1 = y2

let orient ((x1, y1), (x2, y2)) = (min x1 x2, min y1 y2), (max x1 x2, max y1 y2)

let intersect segment1 segment2 =
    // Not my proudest accomplishment
    if isHorizontal segment1 && isVertical segment2 then
        let (x, ly), (_, uy) = orient segment1
        let (lx, y), (ux, _) = orient segment2
        if x > lx && x < ux && y > ly && y < uy then
            Some (x, y)
        else None
    else if isVertical segment1 && isHorizontal segment2 then
        let (lx, y), (ux, _) = orient segment1
        let (x, ly), (_, uy) = orient segment2
        if x > lx && x < ux && y > ly && y < uy then
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

let wiresToAnnotatedSegments (wires : string list) =
    wires
    |> Seq.map (parseWire >> Seq.toList >> wireToSegments (0, 0))
    |> Seq.mapi (fun id segments -> segments |> List.map(fun segment -> id, segment))
    |> Seq.collect id
    |> Seq.toList

let intersections segments =
    Seq.allPairs segments segments
    |> Seq.filter (fun ((id1, s1), (id2, s2)) -> id1 <> id2)
    |> Seq.choose (fun ((id1, s1), (id2, s2)) -> intersect s1 s2)
    

let intersectionSteps (rawWires : string list) =
    let wires = rawWires |> List.map (parseWire >> Seq.toList)
    wiresToAnnotatedSegments rawWires
    |> intersections
    |> Seq.choose(fun intersection ->
        wires
        |> Seq.choose (countSteps (0, 0) intersection 0)
        |> Seq.sort
        |> Seq.tryTake 2
        |> Option.map Seq.sum)
    |> Seq.min

let distance =
    wiresToAnnotatedSegments
    >> intersections
    >> Seq.map (fun (x, y) -> abs x + abs y)
    >> Seq.min
