namespace TestUtils

open SixtySeconds.Common.CommonTypes
open SixtySeconds.Domain

module Utils =
    
    open Utils
    let toPositiveNum = PositiveNum.ofInt >> Result.valueOrException
    let toNoEmptyString = NoEmptyString.ofString >> Result.valueOrException

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
            |> Arb.convert toPositiveNum (fun p -> p.Value)
    
    
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
    
    type RightAnswersGenerator =
        
        static member RightAnswers =
            
            [1..100]
            |> Gen.elements
            |> Gen.map (fun length -> Array.create length true)
            |> Gen.map (Answers.ofBoolSeq)
            |> Arb.fromGen
    
    type AnswersStrikeGenerator =
        
        static member AnswersStrike =
            
            gen {
                let! answers =
                    Arb.generate<Answer>
                    |> Gen.listOf
                    |> Gen.map Answers.ofSeq
                
                let! strike = Arb.generate<StrikeType>
                
                return strike, answers
            } |> Arb.fromGen
    
    type AnswersGenerator =
        
        static member AnswersStrike =
            
            Arb.generate<Answer>
            |> Gen.listOf
            |> Gen.map Answers.ofSeq
            |> Arb.fromGen
            
    type AnswersAndFirstNElementsGenerator =
        
        static member AnswersAndFirstN =
            
            gen {
                let! answers =
                    Arb.generate<Answer>
                    |> Gen.nonEmptyListOf
                    |> Gen.map Answers.ofSeq
                    
                let! firstN =
                    [1..Answers.count answers]
                    |> Gen.elements
                    
                return (answers, firstN)
            } |> Arb.fromGen
            
    
    
    let getId =
        let mutable lastId = 0
        fun () ->
            lastId <- lastId + 1
            lastId |> toPositiveNum 
    
    let teamGenerator =
        Arb.generate<string>
        |> Gen.filter (String.isEmpty >> not)
        |> Gen.map (fun s -> getId(), toNoEmptyString s)
        |> Gen.map (fun (num, name) -> {ID = num; Name = name})
    
    type GameDayType =
        
        static member GameDays =
                
            let answerGenerator packageSize =
                Arb.generate<Answer>
                |> Gen.listOfLength packageSize
                |> Gen.map (Answers.ofSeq)
                
            gen {
                
                let! teamsCount = Gen.choose (1, 100)
                let! packageSize = Gen.choose (1, 50)
                
                let teams =
                        teamGenerator |> Gen.sample teamsCount |> List.ofArray
                
                let allAnswers = 
                    answerGenerator packageSize
                    |> Gen.sample teamsCount |> List.ofArray
                
                return {
                    Tournament =
                        {
                            City = "Unit City" |> NoEmptyString.ofConstString
                            League = "Test league" |> NoEmptyString.ofConstString
                            Season = "Pilot season" |> NoEmptyString.ofConstString
                        }
                    Name = System.DateTime.Now.ToString() |> toNoEmptyString
                    Answers =
                        allAnswers
                        |> List.zip teams
                        |> List.fold (fun map (team, answers) -> map |> Map.add team answers) Map.empty
                    PackageSize = packageSize |> toPositiveNum
                }
            } |> Arb.fromGen
            
    type RatingGenerator =
        
        static member Rating =
            
            gen {
                let! teamsCount = Gen.choose (1, 100)
                
                let rating() =
                     Gen.choose (0, 100)
                     |> Gen.sample 1
                     |> Array.head
                
                let teamsWithRating =
                        teamGenerator
                        |> Gen.map (fun t -> t, rating())
                        |> Gen.sample teamsCount
                        |> List.ofArray
                        
                return teamsWithRating
            } |> Arb.fromGen