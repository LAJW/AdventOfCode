module Extensions
open System

module String =
    let join (separator : string) (values : string seq) = String.Join(separator, values)
    let split (separator : char) (value : string) = value.Split(separator)
    let padLeftWith (totalWidth : int) (padding : char) (value : string) =
        value.PadLeft(totalWidth, padding)

module Seq =
    let replaceAt index newValue list =
        list |> Seq.mapi(fun i value -> if i = index then newValue else value)
    
    let tryTake n elements =
        // Horribly inefficient - consumes the entire sequence
        if (Seq.length elements) < n then None
        else elements |> Seq.take n |> Some

module List =
    let replaceAt index newValue (list : list<'u>) =
        Seq.replaceAt index newValue list |> Seq.toList

module Array2D =
    let toSeq (array : 'u[,]) = seq {
            for x in 0..(array.GetLength(0) - 1) do
                for y in 0..(array.GetLength(1) - 1) do
                    yield (x, y), array.[x, y]
        }



