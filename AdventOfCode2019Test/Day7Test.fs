﻿namespace Test.Day7
open Microsoft.VisualStudio.TestTools.UnitTesting
open Test.Utils

[<TestClass>]
type All() =
    [<TestMethod>]
    member this.Test1() =
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        |> Day5.deserialize
        |> Day7.findHighestOutput
        |> Assert.equal 43210L

    [<TestMethod>]
    member this.Test2() =
        "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        |> Day5.deserialize
        |> Day7.findHighestOutput
        |> Assert.equal 54321L 

    [<TestMethod>]
    member this.Test3() =
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        |> Day5.deserialize
        |> Day7.findHighestOutput
        |> Assert.equal 65210L

    [<TestMethod>]
    member this.Test4() =
        let result = "0222112222120000" |> Day8.decode 2 2 |> Seq.toArray
        CollectionAssert.AreEqual([| 0; 1; 1; 0 |], result)