module Day10
open Extensions
open System.Diagnostics
open VecI

let parse width height (input : string) =
    Array2D.init width height (fun x y -> input.[x + y * height] = '#')
    

let findBestStationLocation (map : bool[,]) =
    let width = map.GetLength 0
    let height = map.GetLength 1
    let birange n = seq { -n..n }
    let within (pos : VecI) =
        pos.X >= 0 && pos.X < width && pos.Y >= 0 && pos.Y < height
    let manhattan (vec1 : VecI) (vec2 : VecI) =
        abs(vec1.X - vec2.X) + abs(vec1.Y - vec2.Y)
    let raysFor (center : VecI) =
        (seq {
            let taken = Array2D.init width height (fun _ _ -> false)
            let reserve (offset : VecI) =
                for a in 1..(max width height) do
                    let pos = center + offset * a
                    if within pos then
                        do taken.[pos.X, pos.Y] <- true
            let offsets = 
                Seq.allPairs (birange width) (birange height)
                |> Seq.map VecI
                |> Seq.sortBy (manhattan VecI.Zero)
                |> Seq.filter ((<>) VecI.Zero)
            for offset in offsets do
                let pos = offset + center
                if within pos && not taken.[pos.X, pos.Y] then
                    yield pos
                    do reserve offset
        })
    let rocks = map |> Array2D.toSeq |> Seq.filter snd |> Seq.map (fst >> VecI)
    rocks
    |> Seq.maxBy(fun center ->
        raysFor center
        |> Seq.filter (fun ray ->
            seq {1..(max width height)}
            |> Seq.map(fun a -> center + (ray - center) * a)
            |> Seq.takeWhile (fun pos -> within pos)
            |> Seq.exists (fun pos -> map.[pos.X, pos.Y])
        )
        |> Seq.length
    )

