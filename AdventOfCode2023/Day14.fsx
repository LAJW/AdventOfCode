#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open System.Collections.Generic

open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let enumerate seq = seq |> Seq.mapi (fun i v -> i, v)

let data = """""" |> String.split ["\n"] |> toList

let data = File.ReadAllLines "./AdventOfCode2023/data14.txt" |> toList
