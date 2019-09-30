namespace Answers

open NUnit.Framework
open System

[<TestFixture>]
module AnswersTests =
    
    open Domain
    open Utils
    open TestUtils
    
    [<Test>]
    let ``getAnswer. Questions count 5. Throws exception on 6``() = 
        
        let answers = Answers.ofBoolArray [|true; false; true; false; true;|]
                    
        let sixthQuestion = 6 |> PositiveNum.ofInt
        
        try
            let answerOnSixthQuestion = answers |> Answers.getAnswer sixthQuestion
            NUnitAssert.Fail()
        with 
            | :? ArgumentException as ex ->
                NUnitAssert.isTrue <| StringUtils.containsSubstring "There are only " ex.Message
            | _ -> NUnitAssert.Fail()