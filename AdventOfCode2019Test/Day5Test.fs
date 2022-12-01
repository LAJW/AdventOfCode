namespace Test.Day5
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type Part2() =
    [<TestMethod>]
    member this.JumpIfElse() =
        let program = Day5.deserialize "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        program |> Day5.run [1L] |> Assert.equal [1L]

    [<TestMethod>]
    member this.NotJumpIfElse() =
        let program = Day5.deserialize "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        program |> Day5.run [0L] |> Assert.equal [0L]

    [<TestMethod>]
    member this.JumpIfElseImmediate() =
        let program = Day5.deserialize "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        program |> Day5.run [1L] |> Assert.equal [1L]

    [<TestMethod>]
    member this.NotJumpIfElseImmediate() =
        let program = Day5.deserialize "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        program |> Day5.run [0L] |> Assert.equal [0L]

    [<TestMethod>]
    member this.EqualTo8() =
        Day5.deserialize "3,9,8,9,10,9,4,9,99,-1,8"
        |> Day5.run [8L] |> Assert.equal [1L]
        
    [<TestMethod>]
    member this.NotEqualTo8() =
        Day5.deserialize "3,9,8,9,10,9,4,9,99,-1,8"
        |> Day5.run [1L] |> Assert.equal [0L]

    [<TestMethod>]
    member this.LessThan8() =
        Day5.deserialize "3,9,7,9,10,9,4,9,99,-1,8"
        |> Day5.run [1L] |> Assert.equal [1L]

    [<TestMethod>]
    member this.NotLessThan8() =
        Day5.deserialize "3,9,7,9,10,9,4,9,99,-1,8"
        |> Day5.run [8L] |> Assert.equal [0L]

    [<TestMethod>]
    member this.EqualTo8Immediate() =
        Day5.deserialize "3,3,1108,-1,8,3,4,3,99"
        |> Day5.run [8L] |> Assert.equal [1L]

    [<TestMethod>]
    member this.NotEqualTo8Immediate() =
        Day5.deserialize "3,3,1108,-1,8,3,4,3,99"
        |> Day5.run [1L] |> Assert.equal [0L]

    [<TestMethod>]
    member this.LessThan8Immediate() =
        Day5.deserialize "3,3,1107,-1,8,3,4,3,99"
        |> Day5.run [7L] |> Assert.equal [1L]

    [<TestMethod>]
    member this.NotLessThan8Immediate() =
        Day5.deserialize "3,3,1107,-1,8,3,4,3,99"
        |> Day5.run [8L] |> Assert.equal [0L]
