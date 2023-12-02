#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System
open FSharpPlus
open System.IO

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = File.ReadAllLines "./AdventOfCode2023/data3.txt"
