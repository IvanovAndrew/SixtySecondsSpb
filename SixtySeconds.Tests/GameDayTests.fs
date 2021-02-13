namespace GameDay.Tests

open SixtySeconds.Common.Errors
open SixtySeconds.Common.CommonTypes
open SixtySeconds.Domain
open SixtySeconds.Actions

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open TestUtils
open Utils


module GameDayUtils =
    
    let createGameDay questionsCount = 
            {
                Tournament =
                    {
                        City = "Unit" |> NoEmptyString.ofConstString
                        League = "Test" |> NoEmptyString.ofConstString
                        Season = "Temp season" |> NoEmptyString.ofConstString
                    }
                Name = Utils.toNoEmptyString <| DateTime.Now.ToString()
                Answers = Map.empty
                PackageSize = questionsCount |> Utils.toPositiveNum
            }
        
module GameDayPropertiesTests =
    
    open FsCheckUtils
    open SixtySeconds.Common.CommonTypes
    
    let createNewTeam teams =
        
        let possibleIds = 
            teams
            |> Seq.length
            |> PositiveNum.ofInt
            |> Result.valueOrException
            |> PositiveNum.next
            |> PositiveNum.createNaturalRange
        
        let usedIDs =     
            teams
            |> Seq.map (fun team -> team.ID)
        
        let newId = 
            possibleIds
            |> Seq.except usedIDs
            |> Seq.head
        
        {ID = newId; Name = Result.valueOrException <| NoEmptyString.ofString "Custom team"}

    let withTeam' team answers gameDay = GameDay.withTeam team answers gameDay |> Result.valueOrException
        
    

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Teams must be unique in the game day`` gameDay =
        
        let existingTeam = gameDay |> GameDay.teams |> Seq.head
        
        let actual = 
            gameDay
            |> GameDay.withTeam existingTeam (gameDay.Answers |> Map.find existingTeam)
            
        match actual with
        | Ok _ -> false
        | Error domainError ->
            match domainError with
            | TeamAlreadyAdded team when team = existingTeam.Name.Value -> true
            | _ -> false
            
        
    [<Property(QuietOnSuccess = true)>]
    let ``Questions package size are equal to questions array length`` num1 num2 =
        
        let precondition num1 num2 = num1 > 0 && num2 > 0 && num1 <> num2
        
        let property questionsCount answersLength =
            
            let gameDay = GameDayUtils.createGameDay questionsCount   
                
            let customTeam = {ID = PositiveNum.numOne; Name = Utils.toNoEmptyString "Test team"}
            let answers = Array.init answersLength (fun _ -> true) |> Answers.ofBoolSeq
            
            match gameDay |> GameDay.withTeam customTeam answers with
            | Ok _ -> false
            | Error domainError ->
                match domainError with
                | QuestionsCountMismatching _ -> true
                | _ -> false
                
        (precondition num1 num2) ==> lazy(property num1 num2)
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Sum right answers by teams is equal to sum right answers by questions`` gameDay =
        
        let teamsRightAnswers =
            gameDay
            |> GameDay.teams
            |> Seq.sumBy (Team.totalAnswered gameDay gameDay.PackageSize)
            
        let questionsRightAnswers =
            gameDay
            |> GameDay.answers
            |> Seq.sumBy Answers.sumRightAnswers
            
        teamsRightAnswers = questionsRightAnswers