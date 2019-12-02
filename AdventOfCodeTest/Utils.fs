module Test.Utils
open Microsoft.VisualStudio.TestTools.UnitTesting

type Assert with
    static member equal (expected : 'u) (actual : 'u) =
        Assert.AreEqual(expected, actual)

