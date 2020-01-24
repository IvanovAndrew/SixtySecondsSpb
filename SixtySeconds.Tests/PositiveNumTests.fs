﻿namespace PositiveNum

open NUnit.Framework
open FsCheck.NUnit
open FsCheck
open TestUtils

[<TestFixture>]
module PropertyBasedTests = 

    open System
    open Utils

    open TestUtils.FsCheckUtils
    

    let createRange x y z =
        let first, step, last =
            x |> PositiveNum.ofInt |> Utils.okValueOrThrow,
            y |> PositiveNum.ofInt |> Utils.okValueOrThrow,
            z |> PositiveNum.ofInt |> Utils.okValueOrThrow
        
        PositiveNum.createRange first step last
    
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<NegativeIntTypes>|])>]    
    let ``PositiveNum property. Positive number can't be created from negative number or zero`` negativeNumber =
        
        match negativeNumber |> PositiveNum.ofInt with 
        | Ok _ -> false
        | Error message -> String.containsSubstring "must be positive" message
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveIntTypes>|])>]
    let ``PositiveNum property. Positive number can be created from positive integers`` positiveInt =
            
            match positiveInt |> PositiveNum.ofInt with 
            | Ok _ -> true
            | _ -> false

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveIntTypes>|])>]        
    let ``PositiveNum property. Value (ofInt (number)) = number`` positiveInt =
        
        let result = 
            positiveInt
            |> PositiveNum.ofInt
            |> Result.map PositiveNum.value
        
        match result with 
        | Ok num -> num = positiveInt
        | _ -> false
        

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. Next positive number is greater`` current =
        
        let nextPositiveNum = current |> PositiveNum.next
        
        let current, next =
            current |> PositiveNum.value,
            nextPositiveNum |> PositiveNum.value
        
        current < next
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. Previous positive num is less`` positiveNum =
        
        let previousNumberIsLess current = 
            let previousPositiveNum = current |> PositiveNum.previous |> Utils.okValueOrThrow
            
            let previousValue, currentValue =
                
                previousPositiveNum |> PositiveNum.value,
                current |> PositiveNum.value
            previousValue < currentValue
        
        positiveNum <> PositiveNum.numOne ==> lazy(previousNumberIsLess positiveNum)
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. Previous (Next (number)) = number`` positiveNum =
        
        let result = 
            positiveNum
            |> PositiveNum.next
            |> PositiveNum.previous
        
        match result with 
        | Ok num -> num = positiveNum
        | _ -> false

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. Next (Previous (number)) = number`` positiveNum =
        
        let reversableProperty x = 
            
            let result = 
                x
                |> PositiveNum.previous
                |> Result.map PositiveNum.next
            
            match result with 
            | Ok num -> num = x
            | Error e -> false
            
        (positiveNum <> PositiveNum.numOne) ==> lazy(reversableProperty positiveNum) 

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. Previous(Next(number)) = Next(Previous (number))`` positiveNum =
        
        let reversableProperty x = 
            
            let previousThenNext =
                x
                |> PositiveNum.previous
                |> Result.map PositiveNum.next
            
            let nextThenPrevious =
                x
                |> PositiveNum.next
                |> PositiveNum.previous
                
            previousThenNext = nextThenPrevious
            
        (positiveNum <> PositiveNum.numOne) ==> lazy(reversableProperty positiveNum)
        

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]        
    let ``PositiveNum property. The first number of natural range is 1`` naturalRangeLength =
        
        let firstItem = naturalRangeLength |> PositiveNum.createNaturalRange |> Seq.head
        
        firstItem = PositiveNum.numOne
        

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]
    let ``PositiveNum property. Natural range is sorted collection`` naturalRangeLength =
        
        let range = PositiveNum.createNaturalRange naturalRangeLength
        
        let sortedRange = range |> List.sortBy PositiveNum.value
        range = sortedRange
            

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]
    let ``PositiveNum property. Length of natural range is equal to the last number``x =
        
        let lastValueOfNaturalRangeIsEqualToLength num =
                
            let rangeLength =
                num
                |> PositiveNum.createNaturalRange
                |> Seq.length
            
            rangeLength = PositiveNum.value num
        
        lastValueOfNaturalRangeIsEqualToLength x
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveNumberTypes>|])>]
    let ``PositiveNum property. Last number of range is maximum value`` x y z =
        
        let precondition =
            let xValue, zValue = x |> PositiveNum.value, z |> PositiveNum.value
            xValue <= zValue
        
        let maximumValueIsLastItem first step last =
            
            let range = PositiveNum.createRange first step last
            
            let lastItem = range |> List.last
            
            let maxValue = range |> List.max
            
            maxValue = lastItem
        
        precondition ==> lazy(maximumValueIsLastItem x y z)

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<PositiveIntTypes>|])>]
    let ``PositiveNum property. All range numbers are unique`` x y z =
        
        let range = createRange x y z
        let uniqueRange = range |> Seq.distinct
        
        Seq.length range = Seq.length uniqueRange 