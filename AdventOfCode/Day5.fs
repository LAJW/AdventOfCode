module Day5
open Extensions
open System
open System.Diagnostics

let narrow (input : int64) =
    let result = int input
    if (int64 result) = input then result
    else invalidArg "input" "Lossy conversion"

let deserialize = String.split ',' >> Seq.map int64 >> Seq.toList
let serialize (input : int64 list) = input |> Seq.map string |> String.join ","

let replaceOrExtendAt index newValue (list : int64 list) =
    if index < list.Length then
        list |> List.replaceAt index newValue
    else
        (list @ List.init (1 + index - list.Length) (fun _ -> 0L))
        |> List.replaceAt index newValue

[<DebuggerDisplay("Program : {Index} {DebuggerOutput}")>]
type State = {
    Index        : int
    RelativeBase : int
    Inputs       : int64 list
    Outputs      : int64 list
    Program      : int64 list
} with
    member this.DebuggerOutput with get() = this.Program |> Seq.map string |> String.join ","

type Mode = Immediate | Positional | Relative
module Mode =
    let parse = function
        | '0' -> Positional
        | '1' -> Immediate
        | '2' -> Relative

type Instruction =
    | Add | Multiply | Read | Write | JumpIfTrue | JumpIfFalse | LessThan
    | Equals | AdjustRelativeBase | Stop

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
        | AdjustRelativeBase -> 1
        | Stop -> 0

let parseOpcode(longOpcode : int64) : Instruction * (Mode list) =
    let opcode = narrow longOpcode
    let instruction = match opcode % 100 with
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> Read
        | 4 -> Write
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
        | 9 -> AdjustRelativeBase
        | 99 -> Stop
    if instruction = Stop then
        instruction, []
    else
        let modes = (string (opcode / 100))
                    |> String.padLeftWith (Instruction.paramCount instruction) '0'
                    |> Seq.map Mode.parse
                    |> Seq.rev
                    |> Seq.toList
        instruction, modes
        

type Argument(state : State, mode : Mode, value : int64) =
    let tryGet index list =
        if index < List.length list
        then Some list.[index]
        else None

    // Value of an argument ignoring value's mode
    member this.Raw =
        match mode with 
        | Relative -> value + (int64 state.RelativeBase)
        | Immediate -> value
        | Positional -> value

    // Value of an argument respecting value's mode
    member this.Value =
        let program = state.Program
        match mode with 
        | Relative -> program |> tryGet (state.RelativeBase + narrow value) |> Option.defaultValue 0L
        | Immediate -> value
        | Positional -> program |> tryGet (narrow value) |> Option.defaultValue 0L

let runFull (inputs : int64 list) (program : int64 list) : State =
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
            { state with Program = program |> replaceOrExtendAt (narrow output.Raw) (func a.Value b.Value)
                         Index = nextIndex }
        let jumpIf func state = match values with [condition; jumpTo] ->
            if func condition.Value then { state with Index = narrow jumpTo.Value }
            else { state with Index = nextIndex }
        match instruction with
        | Add -> state |> apply (+) |> step
        | Multiply -> state |> apply (*) |> step
        | Read -> step {
            state with Program = program |> replaceOrExtendAt (narrow values.[0].Raw) (List.head inputs)
                       Inputs = List.tail inputs
                       Index = nextIndex }
        | Write -> step {
            state with Outputs = outputs @ [ values.[0].Value ]
                       Index = nextIndex }
        | JumpIfTrue -> state |> jumpIf ((<>) 0L) |> step
        | JumpIfFalse -> state |> jumpIf ((=) 0L) |> step
        | LessThan -> state |> apply (fun a b -> if a < b then 1L else 0L) |> step
        | Equals -> state |> apply (fun a b -> if a = b then 1L else 0L) |> step
        | AdjustRelativeBase ->
            step { state with RelativeBase = state.RelativeBase + (narrow values.[0].Value)
                              Index = nextIndex }
        | Stop -> state
    step { Index = 0; RelativeBase = 0; Inputs = inputs; Outputs = []; Program = program }

let run (inputs : int64 list) (program : int64 list) : int64 list =
    (runFull inputs program).Outputs
