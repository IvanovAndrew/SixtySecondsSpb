namespace Answers

open NUnit.Framework
open FsUnit
open System

[<TestFixture>]
module AnswersTests =
    
    open Domain
    open Utils
    
    [<Test>]
    let ``getAnswer. Questions count 5. Throws exception on 6``() = 
        
        let answers = Answers.ofBoolArray [|true; false; true; false; true;|]
                    
        let sixthQuestion = 6 |> PositiveNum.ofInt
        
        (fun () -> answers |> Answers.getAnswer sixthQuestion |> ignore)
        |> should throw typeof<ArgumentException> 