module Day8

let decode width height (image : string) =
    image
    |> Seq.map (fun char -> char.ToString() |> int)
    |> Seq.chunkBySize (width * height)
    |> Seq.rev
    |> Seq.reduce (fun background foreground ->
        Seq.zip background foreground
        |> Seq.map (fun (bg, fg) -> if fg = 2 then bg else fg)
        |> Seq.toArray)
