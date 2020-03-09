namespace TestUtils
open Domain

module Utils = 
    let okValueOrThrow res = 
        match res with 
        | Ok value -> value
        | Error e -> invalidOp e

module FsCheckUtils =
    
    open FsCheck
    open Utils
    
    type NegativeIntTypes =
        static member NegativeNumbers =
            
            Arb.generate<NegativeInt>
            |> Gen.map (fun (NegativeInt i) -> i)
            |> Arb.fromGen
    
    type PositiveIntTypes =
        static member PositiveNumbers =
            
            Arb.generate<PositiveInt>
            |> Gen.map (fun (PositiveInt i) -> i)
            |> Arb.fromGen
    
    type PositiveNumberTypes =
        static member ValidIntegers =
            [1 .. 100]
            |> Gen.elements 
            |> Arb.fromGen
            |> Arb.convert (PositiveNum.ofInt >> okValueOrThrow) PositiveNum.value
    
    
    type NonEmptySeq =
        static member ValidSeq =
            [1..10]
            |> Gen.elements 
            |> Gen.nonEmptyListOf
            |> Gen.map Seq.ofList
            |> Arb.fromGen
            
    type SeqResult =
        
        static member ValidResults =
            
            Arb.generate<Result<_, _>>
            |> Arb.fromGen
        
    // https://stackoverflow.com/a/31069606/2486842
    let isOk = function Ok _ -> true | _ -> false
    let g = Arb.generate<Result<int, string>> |> Gen.filter isOk |> Gen.listOf
    type SeqOKResult =
        
        static member ValidResults =
            
            Arb.fromGen g
            
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
                    lastId <- lastId + 1
                    okValueOrThrow <| PositiveNum.ofInt lastId
            
            let teamGenerator =
                Arb.generate<string>
                |> Gen.filter (String.isEmpty >> not)
                |> Gen.map (fun s -> getId(), okValueOrThrow <| NoEmptyString.ofString s)
                |> Gen.map (fun (num, name) -> {ID = num; Name = name})
                
            let answerGenerator packageSize =
                Arb.generate<Answer>
                |> Gen.listOfLength packageSize
                |> Gen.map Answers.ofSeq
            
            
            gen {
                
                let teamsCount, packageSize =
                    match [1..100] |> Gen.elements |> Gen.sample 2 with
                    | [|first; second|] -> first, second
                    | x -> failwithf "Wrong generator value %A" x
                
                let teams =
                        teamGenerator |> Gen.sample teamsCount |> List.ofArray
                
                let allAnswers = 
                    answerGenerator packageSize
                    |> Gen.sample teamsCount |> List.ofArray
                
                return {
                    Tournament = "Generated tournament" |> NoEmptyString.ofString |> okValueOrThrow
                    Name = System.DateTime.Now.ToString() |> NoEmptyString.ofString |> okValueOrThrow
                    Answers =
                        allAnswers
                        |> List.zip teams
                        |> List.fold (fun map (team, answers) -> map |> Map.add team answers) Map.empty
                    PackageSize = packageSize |> PositiveNum.ofInt |> okValueOrThrow
                }
            } |> Arb.fromGen