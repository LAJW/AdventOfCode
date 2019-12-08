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
        

type Argument(state : Memory, mode : Mode, value : int) =
    // Value of an argument ignoring value's mode
    member this.Raw = value

    // Value of an argument respecting value's mode
    member this.Value =
        let program = state.Program
        match mode with 
        | Immediate -> value
        | Positional -> program.[value]

let run (inputs : int list) (program : int list) : (int list) =
    let rec step (memory : Memory) : Memory =
        let program = memory.Program
        let index = memory.Index
        let inputs = memory.Inputs
        let outputs = memory.Outputs
        let instruction, modes = parseOpcode program.[index]
        let values =
            Seq.zip (program |> Seq.skip (index + 1)) modes
            |> Seq.map (fun (value, mode) -> Argument(memory, mode, value))
            |> Seq.toList
        let nextIndex = index + 1 + (Seq.length modes)
        let apply func memory = match values with [a; b; output] ->
            { memory with Program = program |> List.replaceAt (output.Raw) (func a.Value b.Value)
                          Index = nextIndex }
        let jumpIf func memory = match values with [condition; jumpTo] ->
            if func condition.Value then { memory with Index = jumpTo.Value }
            else { memory with Index = nextIndex }
        match instruction with
        | Add -> memory |> apply (+) |> step
        | Multiply -> memory |> apply (*) |> step
        | Read -> step {
            memory with Program = program |> List.replaceAt (values.[0].Raw) (List.head inputs)
                        Inputs = List.tail inputs
                        Index = nextIndex }
        | Write -> step {
            memory with Outputs = outputs @ [ values.[0].Value ]
                        Index = nextIndex }
        | JumpIfTrue -> memory |> jumpIf ((<>) 0) |> step
        | JumpIfFalse -> memory |> jumpIf ((=) 0) |> step
        | LessThan -> memory |> apply (fun a b -> if a < b then 1 else 0) |> step
        | Equals -> memory |> apply (fun a b -> if a = b then 1 else 0) |> step
        | Stop -> memory
    let endState = step { Index = 0; Inputs = inputs; Outputs = []; Program = program }
    endState.Outputs

