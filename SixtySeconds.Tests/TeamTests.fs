namespace TeamTests

open FsCheck
open FsCheck.NUnit
open TestUtils.FsCheckUtils

module ``TeamTests`` =
    
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    open Utils
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
        
        {ID = newId; Name = "Custom team" |> NoEmptyString.ofString |> Result.valueOrException}
        
    let withTeam' team answers gameDay = GameDay.withTeam team answers gameDay |> Result.valueOrException
    
    let allWrongAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> false) 
        |> Answers.ofBoolSeq
        
        
    let allRightAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> true) 
        |> Answers.ofBoolSeq
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``All team answers are right. Total answered questions is equal to questions count`` gameDay =
        
        let customTeam = gameDay |> GameDay.teams |> createNewTeam
        let answers = allRightAnswers (PositiveNum.value gameDay.PackageSize) 
        
        let gameDayWithCustomTeam = 
            gameDay
            |> withTeam' customTeam answers
        
        let totalAnswered = Team.totalAnswered gameDayWithCustomTeam gameDay.PackageSize customTeam
        
        PositiveNum.value gameDayWithCustomTeam.PackageSize = int totalAnswered

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``All team answers are wrong. Total answered is 0`` gameDay =
        
        let loserTeam = gameDay |> GameDay.teams |> createNewTeam
        
        let gameDayWithLoserTeam =
            
            let answers =
                gameDay.PackageSize
                |> PositiveNum.value 
                |> allWrongAnswers
            
            gameDay
            |> withTeam' loserTeam answers
        
        let totalAnswered = Team.totalAnswered gameDayWithLoserTeam gameDayWithLoserTeam.PackageSize loserTeam
        
        totalAnswered = 0<RightAnswer>
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Leading team has 0 gap from the first place`` gameDay =
        
        let leader =
            gameDay
            |> Rating.ofGameDay
            |> Rating.leadingTeams PositiveNum.numOne
            |> Seq.head
        
        let gap = Team.getGapFromTheFirstPlace gameDay leader gameDay.PackageSize
        gap = 0<RightAnswer>