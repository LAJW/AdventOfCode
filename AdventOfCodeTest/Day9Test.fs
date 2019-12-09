namespace Test.Day9
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type All() =

    [<TestMethod>]
    member this.OutputCopyOfItself() =
        "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        |> Day5.deserialize
        |> Day5.run []
        |> Day5.serialize
        |> Assert.equal "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        
    [<TestMethod>]
    member this.Output16DigitNumber() =
        "1102,34915192,34915192,7,4,7,99,0"
        |> Day5.deserialize
        |> Day5.run []
        |> Day5.serialize
        |> String.length
        |> Assert.equal 16

    [<TestMethod>]
    member this.OutputTheLargeNumberInTheMiddle() =
        "104,1125899906842624,99"
        |> Day5.deserialize
        |> Day5.run []
        |> Assert.equal [1125899906842624L]

