module Day5
open Extensions
open System
open System.Diagnostics

let deserialize = String.split ',' >> Seq.map int >> Seq.toList
let serialize (input : int list) = input |> Seq.map string |> String.join ","

[<DebuggerDisplay("Program : {Index} {DebuggerOutput}")>]
type State = {
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
        

type Argument(state : State, mode : Mode, value : int) =
    // Value of an argument ignoring value's mode
    member this.Raw = value

    // Value of an argument respecting value's mode
    member this.Value =
        let program = state.Program
        match mode with 
        | Immediate -> value
        | Positional -> program.[value]

let runFull (inputs : int list) (program : int list) : State =
    let rec step (state : State) : State =
        let program = state.Program
        let index = state.Index
        let inputs = state.Inputs
        let outputs = state.Outputs
        let instruction, modes = parseOpcode program.[index]
        let values =
            Seq.zip (program |> Seq.skip (index + 1)) modes
            |> Seq.map (fun (value, mode) -> Argument(state, mode, value))
            |> Seq.toList
        let nextIndex = index + 1 + (Seq.length modes)
        let apply func state = match values with [a; b; output] ->
            { state with Program = program |> List.replaceAt (output.Raw) (func a.Value b.Value)
                         Index = nextIndex }
        let jumpIf func state = match values with [condition; jumpTo] ->
            if func condition.Value then { state with Index = jumpTo.Value }
            else { state with Index = nextIndex }
        match instruction with
        | Add -> state |> apply (+) |> step
        | Multiply -> state |> apply (*) |> step
        | Read -> step {
            state with Program = program |> List.replaceAt (values.[0].Raw) (List.head inputs)
                       Inputs = List.tail inputs
                       Index = nextIndex }
        | Write -> step {
            state with Outputs = outputs @ [ values.[0].Value ]
                       Index = nextIndex }
        | JumpIfTrue -> state |> jumpIf ((<>) 0) |> step
        | JumpIfFalse -> state |> jumpIf ((=) 0) |> step
        | LessThan -> state |> apply (fun a b -> if a < b then 1 else 0) |> step
        | Equals -> state |> apply (fun a b -> if a = b then 1 else 0) |> step
        | Stop -> state
    step { Index = 0; Inputs = inputs; Outputs = []; Program = program }

let run (inputs : int list) (program : int list) = (runFull inputs program).Outputs

