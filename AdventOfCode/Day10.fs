module Day10
open Extensions
open System.Diagnostics

let parse width height (input : string) =
    Array2D.init width height (fun x y -> input.[x + y * height] = '#')
    

let findBestStationVisibleAsteroidCount (map : bool[,]) =
    let width = map.GetLength 0
    let height = map.GetLength 1
    let birange n = seq { -n..n }
    let within (x, y) =
        x >= 0 && x < width && y >= 0 && y < height
    let manhattan (x1, y1) (x2, y2) =
        abs(x1 - x2) + abs(y1 - y2)
    let raysFor baseX baseY =
        (seq {
            let taken = Array2D.init width height (fun _ _ -> false)
            let reserve (offsetX, offsetY) =
                for a in 1..(max width height) do
                    let x, y = baseX + offsetX * a, baseY + offsetY * a
                    if within (x, y) then
                        do taken.[x, y] <- true
            let offsets = 
                Seq.allPairs (birange width) (birange height)
                |> Seq.sortBy(manhattan (0, 0))
                |> Seq.filter ((<>) (0, 0))
            for (offsetX, offsetY) in offsets do
                let x, y = (offsetX + baseX), (offsetY + baseY)
                if within(x, y) && not taken.[x, y] then
                    yield x, y
                    do reserve (offsetX, offsetY)
        })
    let rocks = map |> Array2D.toSeq |> Seq.filter snd |> Seq.map fst
    rocks
    |> Seq.map(fun (baseX, baseY) ->
        raysFor baseX baseY
        |> Seq.filter (fun (x, y) ->
            seq {1..(max width height)}
            |> Seq.map(fun a ->
                let x = baseX + (x - baseX) * a
                let y = baseY + (y - baseY) * a
                (x, y))
            |> Seq.takeWhile (fun pos -> within pos)
            |> Seq.exists (fun (x, y) -> map.[x, y])
        )
        |> Seq.length
    )
    |> Seq.max

