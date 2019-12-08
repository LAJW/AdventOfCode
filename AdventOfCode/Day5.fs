module Day5
open Extensions
open System
open System.Diagnostics

let deserialize = String.split ',' >> Seq.map int >> Seq.toList
let serialize (input : int list) = input |> Seq.map string |> String.join ","

[<DebuggerDisplay("Program : {Index} {DebuggerOutput}")>]
type Memory = {
    Index   : int
    Inputs  : int list
    Outputs : int list
    Program : int list
} with
    member this.DebuggerOutput with get() = this.Program |> Seq.map string |> String.join ","

type Mode = Immediate | Positional
type Instruction = Add | Multiply | Read | Write | JumpIfTrue | JumpIfFalse | LessThan | Equals | Stop
module Instruction =
    let paramCount = function
        | Add -> 3
        | Multiply -> 3
        | Read -> 1
        | Write -> 1
        | JumpIfTrue -> 2
        | JumpIfFalse -> 2
        | LessThan -> 3
        | Equals -> 3
        | Stop -> 0

let parseOpcode(opcode : int) : Instruction * (Mode list) =
    let instruction = match opcode % 100 with
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> Read
        | 4 -> Write
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
        | 99 -> Stop
    if instruction = Stop then
        instruction, []
    else
        let modes = (string (opcode / 100))
                    |> String.padLeftWith (Instruction.paramCount instruction) '0'
                    |> Seq.map (function | '0' -> Positional | '1' -> Immediate)
                    |> Seq.rev
                    |> Seq.toList
        instruction, modes
        

let run (inputs : int list) (program : int list) : (int list) =
    let rec step (memory : Memory) : Memory =
        let program = memory.Program
        let index = memory.Index
        let inputs = memory.Inputs
        let outputs = memory.Outputs
        let instruction, modes = parseOpcode program.[index]
        let values =
            Seq.zip (program |> Seq.skip (index + 1)) modes
            |> Seq.map (fun (value, mode) ->
                match mode with
                | Immediate -> value
                | Positional -> program.[value]
            )
            |> Seq.toList
        let nextIndex = index + 1 + (Seq.length modes)
        match instruction with
        | Add -> step {
            memory with Program = program |> List.replaceAt (program.[index + 3]) (values.[0] + values.[1])
                        Index = nextIndex }
        | Multiply -> step {
            memory with Program = program |> List.replaceAt (program.[index + 3]) (values.[0] * values.[1])
                        Index = nextIndex }
        | Read -> step {
            memory with Program = program |> List.replaceAt (program.[index + 1]) (List.head inputs)
                        Inputs = List.tail inputs
                        Index = nextIndex }
        | Write -> step {
            memory with Outputs = outputs @ [ values.[0] ]
                        Index = nextIndex }
        | JumpIfTrue ->
            if values.[0] <> 0 then step { memory with Index = values.[1] }
            else step { memory with Index = index + 3 }
        | JumpIfFalse ->
            if values.[0] = 0 then step { memory with Index = values.[1] }
            else step { memory with Index = index + 3 }
        | LessThan -> step {
            memory with Program = program |> List.replaceAt program.[index + 3] (if values.[0] < values.[1] then 1 else 0)
                        Index = nextIndex }
        | Equals -> step {
            memory with Program = program |> List.replaceAt program.[index + 3] (if values.[0] = values.[1] then 1 else 0)
                        Index = nextIndex }
        | Stop -> memory
    let endState = step { Index = 0; Inputs = inputs; Outputs = []; Program = program }
    endState.Outputs

