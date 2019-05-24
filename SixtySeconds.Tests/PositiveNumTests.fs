namespace PositiveNum.Tests

open FsCheck
open FsCheck.NUnit

open NUnit.Framework

open Utils

open System

module NUnitAssert = 
    
    let areEqual a b = Assert.AreEqual(a, b)
    let trueForAll seq f = 
        let res = 
            seq
            |> Seq.forall f
        Assert.True res

    

[<TestFixture>]
module PositiveNumTests = 
    
    open NUnitAssert

    [<Test>]
    let ``Positive num. Create of negative number. Should failed``() =
        
        try
            -1 |> PositiveNum.ofInt |> ignore
            Assert.Fail()
        with 
            | :? ArgumentException -> Assert.Pass()
            | _ -> Assert.Fail()

    [<Test>]
    let ``Positive num. Create of zero. Should failed``() = 

        try
            0 |> PositiveNum.ofInt |> ignore
            Assert.Fail()
        with 
            | :? ArgumentException -> Assert.Pass()
            | _ -> Assert.Fail()

    [<Test>]
    let ``Positive num. Create of positive num. Should OK``() = 
        
        let one = 1 |> PositiveNum.ofInt
        Assert.Pass()

    // TODO Add FsCheck here
    [<Test>]
    let ``Create natural range. First item is 1``() = 

        let range = 
            42
            |> PositiveNum.ofInt
            |> PositiveNum.createNaturalRange

        let head = range |> List.head

        let expected = 1 |> PositiveNum.ofInt

        NUnitAssert.areEqual expected head

    // TODO Add FsCheck here
    [<Test>]
    let ``Create natural range. Length of range is equal to last value``() = 
        
        let rangeLength = 
            42
            |> PositiveNum.ofInt
            |> PositiveNum.createNaturalRange
            |> List.length

        NUnitAssert.areEqual 42 rangeLength

    // TODO Add FsCheck here
    [<Test>]
    let ``Create range. First value is firstParam``() = 
        
        let first, step, last = 
            3 |> PositiveNum.ofInt, 5 |> PositiveNum.ofInt, 42 |> PositiveNum.ofInt

        let firstElement = 
            last
            |> PositiveNum.createRange first step
            |> List.head

        NUnitAssert.areEqual first firstElement

    // TODO Add FsCheck here
    [<Test>]
    let ``Create range. All values are not greater than last param``() = 
        
        let first, step, last = 
            2 |> PositiveNum.ofInt, 5 |> PositiveNum.ofInt, 42 |> PositiveNum.ofInt

        let range = 
            last
            |> PositiveNum.createRange first step

        NUnitAssert.trueForAll range (fun x -> x <= last)

    // TODO Add FsCheck here
    [<Test>]
    let ``Create range. Returns sorted list``() = 
        
        let first, step, last = 
            2 |> PositiveNum.ofInt, 5 |> PositiveNum.ofInt, 42 |> PositiveNum.ofInt

        let range = 
            last
            |> PositiveNum.createRange first step

        let sorted = range |> List.sort

        NUnitAssert.areEqual sorted range