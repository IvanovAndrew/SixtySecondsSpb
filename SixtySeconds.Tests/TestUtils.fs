namespace TestUtils
open Domain

[<RequireQualifiedAccess>]
module NUnitAssert = 

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


module FsCheckUtils =
    
    open FsCheck
    open Utils
    open Domain
    
    type PositiveNumberTypes =
        static member ValidIntegers =
            [1 .. 100]
            |> Gen.elements 
            |> Arb.fromGen
            |> Arb.convert PositiveNum.ofInt PositiveNum.value
            
            
    type AnswersTypes =
        static member ValidAnswers =
            Arb.generate<Answer>
            |> Gen.listOfLength 12
            |> Arb.fromGen
    
    type GameDayType =
        
        static member GameDays =
            
            let getId =
                let mutable lastId = 0
                fun () ->
                    lastId <-lastId + 1
                    PositiveNum.ofInt lastId
            
            let teamGenerator =
                Arb.generate<string>
                |> Gen.filter (StringUtils.isEmpty >> not)
                |> Gen.map (fun s -> getId(), NoEmptyString.ofString s)
                |> Gen.map (fun (num, name) -> {ID = num; Name = name})
                
            let answerGenerator =
                Arb.generate<Answer>
                |> Gen.listOfLength 12
                |> Gen.map Answers.ofSeq
            
            
            
            
            gen {
                
                let! team1 = teamGenerator
                let! answers1 = answerGenerator
                
                let! team2 = teamGenerator
                let! answers2 = answerGenerator
                
                let! team3 = teamGenerator
                let! answers3 = answerGenerator
                
                let! team4 = teamGenerator
                let! answers4 = answerGenerator
                
                let! team5 = teamGenerator
                let! answers5 = answerGenerator
                
                return {
                    Day = System.DateTime.Now
                    Answers =
                        [
                            (team1, answers1);
                            (team2, answers2);
                            (team3, answers3);
                            (team4, answers4);
                            (team5, answers5);
                        ]
                        |> List.fold (fun map (team, answers) -> map |> Map.add team answers) Map.empty
                    PackageSize = PositiveNum.ofInt 12
                }
            } |> Arb.fromGen