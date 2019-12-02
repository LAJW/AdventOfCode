module Extensions
open System

module String =
    let join (separator : string) (values : string seq) = String.Join(separator, values)

module Seq =
    let replaceAt index newValue list =
        list |> Seq.mapi(fun i value -> if i = index then newValue else value)

module List =
    let replaceAt index newValue (list : list<'u>) =
        Seq.replaceAt index newValue list |> Seq.toList



