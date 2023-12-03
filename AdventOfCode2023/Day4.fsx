#r "./bin/Debug/net7.0/FSharpPlus.dll"

open System

open System.Text.RegularExpressions
open FSharpPlus
open System.IO

let inline value(self : ^T when ^T : (member Value : ^V)) = (^T : (member Value : ^V) self)

let join (delim : string) (strs : string seq) = String.Join(delim, strs)

let data = """""".Split("\n")

let data = File.ReadAllLines "./AdventOfCode2023/data4.txt"
