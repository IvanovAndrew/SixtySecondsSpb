namespace TestUtils
open Domain

module FsCheckUtils =
    
    open FsCheck
    open Utils
    
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
                
            let answerGenerator packageSize =
                Arb.generate<Answer>
                |> Gen.listOfLength packageSize
                |> Gen.map Answers.ofSeq
            
            
            
            
            gen {
                
                let teamsCount, packageSize =
                    match [1..100] |> Gen.elements |> Gen.sample 2 with
                    | [|t; p|] -> t, p
                    | x -> failwithf "Wrong generator value %A" x
                
                let teams =
                        teamGenerator |> Gen.sample teamsCount |> List.ofArray
                
                let allAnswers = 
                    answerGenerator packageSize
                    |> Gen.sample teamsCount |> List.ofArray
                
                return {
                    Day = System.DateTime.Now
                    Answers =
                        allAnswers
                        |> List.zip teams
                        |> List.fold (fun map (team, answers) -> map |> Map.add team answers) Map.empty
                    PackageSize = PositiveNum.ofInt packageSize
                }
            } |> Arb.fromGen