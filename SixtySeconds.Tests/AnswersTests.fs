namespace Answers

open NUnit.Framework
open FsUnit
open System
open TestUtils

[<TestFixture>]
module AnswersTests =
    
    open Domain
    open Utils
    
    [<Test>]
    let ``getAnswer. Questions count 5. Throws exception on 6``() = 
        
        let answers = Answers.ofBoolArray [|true; false; true; false; true;|]
                    
        let sixthQuestion = 6 |> PositiveNum.ofInt |> okValueOrThrow
        
        (fun () -> answers |> Answers.getAnswer sixthQuestion |> ignore)
        |> should throw typeof<ArgumentException> 