[<RequireQualifiedAccess>]
module NUnitAssert

open NUnit.Framework
    
let areEqual a b = Assert.AreEqual(a, b)
let isTrue (a : bool) = Assert.IsTrue(a)
let trueForAll seq f = 
    let res = 
        seq
        |> Seq.forall f
    Assert.True res

let Pass() = Assert.Pass()
let Fail() = Assert.Fail()