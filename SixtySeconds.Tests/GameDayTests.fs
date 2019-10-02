namespace GameDay.Tests

open Domain

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
module GameDayExampleTests = 

    open Domain
    open Utils
    open TestUtils    

    let createTeam id name =
        {
            ID = id |> PositiveNum.ofInt;
            Name = name |> NoEmptyString.ofString
        }
        
    let firstTeam = createTeam 1 "Team1"
    let secondTeam = createTeam 2 "Team2"
    let thirdTeam = createTeam 3 "Team3"
    let fourthTeam = createTeam 4 "Team4"
    
    
    
    let createEmptyGameDay questionsCount =
        {
            Day = DateTime.Now
            Answers = Map.empty
            PackageSize = PositiveNum.ofInt questionsCount
        }
    
    [<Test>]
    let ``LeadingTeams. N is greater than number of teams. Returns all teams``() = 
        
        let gameDay =
            createEmptyGameDay 3
            |> GameDay.withTeam firstTeam (Answers.ofBoolArray [|true; true; true;|])
            |> GameDay.withTeam secondTeam (Answers.ofBoolArray [|false; false; false;|])

        let top3Teams = GameDay.leadingTeams gameDay 3
        let top3Count = top3Teams |> Seq.length
        
        top3Count |> should lessThan 3 

    [<Test>]
    let ``LeadingTeams. Two teams on secondPlace. Returns 3 teams on top2``() = 
        
        let gameDay = 
            createEmptyGameDay 5
            |> GameDay.withTeam firstTeam (Answers.ofBoolArray [|true;true;true;true;true;|])
            |> GameDay.withTeam secondTeam (Answers.ofBoolArray [|true;true;true;true;false;|])
            |> GameDay.withTeam thirdTeam (Answers.ofBoolArray [|true;true;true;true;false;|])
            |> GameDay.withTeam fourthTeam (Answers.ofBoolArray [|true;true;true;false;false;|])
    
        let top2Teams = GameDay.leadingTeams gameDay 2
        let n = top2Teams |> Seq.length
        
        n |> should equal 3 

    [<Test>]
    let ``getAnswer. Team answered right on the first question. Returns Right``() = 
        
        let gameDay = 
            createEmptyGameDay 5
            |> GameDay.withTeam firstTeam (Answers.ofBoolArray [|true;false;false;true;true;|])
            
        let firstQuestion = 1 |> PositiveNum.ofInt
        let answerOnFirstQuestion = GameDay.getAnswer gameDay firstTeam firstQuestion
        
        answerOnFirstQuestion |> should equal Answer.Right 

    [<Test>]
    let ``getAnswer. Team answered wrong on the second question. Returns Wrong``() = 
        
        let gameDay = 
            createEmptyGameDay 5
            |> GameDay.withTeam firstTeam (Answers.ofBoolArray [|true;false;false;true;true;|])
            
        let secondQuestion = PositiveNum.numOne |> PositiveNum.next
        let answerOnSecondQuestion = GameDay.getAnswer gameDay firstTeam secondQuestion
        
        answerOnSecondQuestion |> should equal Answer.Wrong 

     
        
    [<Test>]
    let ``getDistanceFromFirstPlace. First team gives right answer on first question, second team doesn't. Distance is 1``() =
    
        let gameDay =
            createEmptyGameDay 1
            |> GameDay.withTeam firstTeam (Answers.ofBoolArray [|true;|])
            |> GameDay.withTeam secondTeam (Answers.ofBoolArray [|false;|])
            
                    
        let distance = GameDay.getDistanceFromTheFirstPlace gameDay secondTeam PositiveNum.numOne
        distance |> should equal -1 
        
        
module GameDayPropertiesTests =
    
    open TestUtils
    open FsCheckUtils
    open Utils
    
    let createNewTeam teams =
        
        let possibleIds = 
            teams
            |> Seq.length
            |> PositiveNum.ofInt
            |> PositiveNum.next
            |> PositiveNum.createNaturalRange
        
        let usedIDs =     
            teams
            |> Seq.map (fun team -> team.ID)
        
        let newId = 
            possibleIds
            |> Seq.except usedIDs
            |> Seq.head
        
        {ID = newId; Name = NoEmptyString.ofString "Custom team"}
        
    let allWrongAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> false) 
        |> Answers.ofBoolArray
        
        
    let allRightAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> true) 
        |> Answers.ofBoolArray

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Teams that gives only right answers is on the first place`` gameDay =
        
        let customTeam =  gameDay |> GameDay.teams |> createNewTeam 
        
        let gameDay' =
            let packageSize = gameDay.PackageSize
            
            let answers = allRightAnswers <| PositiveNum.value packageSize
            
            {gameDay with Answers = gameDay.Answers |> Map.add customTeam answers}
            
        let place = GameDay.getPlace gameDay' customTeam
        
        place.From = PositiveNum.numOne
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Game day property. Teams that gives only wrong answers is on the last place`` gameDay =
        
        let customTeam = gameDay |> GameDay.teams |> createNewTeam
        
        let gameDay' =
            let packageSize = gameDay.PackageSize
            
            let answers = allWrongAnswers <| PositiveNum.value packageSize
            
            {gameDay with Answers = gameDay.Answers |> Map.add customTeam answers}
        
        let lastPlace = gameDay' |> GameDay.teams |> Seq.length |> PositiveNum.ofInt
            
        let place = GameDay.getPlace gameDay' customTeam
        
        place.To = lastPlace
        
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Leading team has 0 distance from the first place`` gameDay =
        
        let leader = GameDay.leadingTeams gameDay 1 |> Seq.head
        
        let distance = GameDay.getDistanceFromTheFirstPlace gameDay leader gameDay.PackageSize
        distance = 0<RightAnswer>
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Teams must be unique in the game day`` gameDay =
        
        let existingTeam = gameDay |> GameDay.teams |> Seq.head
        
        try 
            gameDay
            |> GameDay.withTeam existingTeam (gameDay.Answers |> Map.find existingTeam)
            |> ignore
            false
        with
            | :? ArgumentException as ex ->
                
                StringUtils.containsSubstring "is already added" ex.Message 
            | _ -> false
            
        
    [<Property(QuietOnSuccess = true)>]
    let ``Game day questions count are equal to questions array length`` num1 num2 =
        
        let precondition num1 num2 = num1 > 0 && num2 > 0 && num1 <> num2
        
        let property questionsCount answersLength =
            
            let gameDay =  
                {
                    Day = DateTime.Now;
                    Answers = Map.empty;
                    PackageSize = PositiveNum.ofInt questionsCount
                }
                
            let customTeam = {ID = PositiveNum.numOne; Name = NoEmptyString.ofString "Test team"}
            let answers = Array.init answersLength (fun _ -> true) |> Answers.ofBoolArray
            
            try 
                let _ =
                    gameDay
                    |> GameDay.withTeam customTeam answers
                false
            with
                | :? ArgumentException as ex ->
                    
                    StringUtils.containsSubstring "answers" ex.Message
                | _ -> false
                
        (precondition num1 num2) ==> lazy(property num1 num2)
        
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``All team answers are wrong. Total answered is 0`` gameDay =
        
        let loserTeam = gameDay |> GameDay.teams |> createNewTeam
         
        
        let gameDayWithLoserTeam =
            
            let answers = allWrongAnswers <| PositiveNum.value gameDay.PackageSize
            
            gameDay
            |> GameDay.withTeam loserTeam answers
        
        let totalAnswered = GameDay.totalAnswered gameDayWithLoserTeam gameDayWithLoserTeam.PackageSize loserTeam
        
        totalAnswered = 0<RightAnswer>
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``All team answers are right. Total answered questions is equal to questions count`` gameDay =
        
        let customTeam = gameDay |> GameDay.teams |> createNewTeam
        let answers = allRightAnswers (PositiveNum.value gameDay.PackageSize) 
        
        let gameDayWithCustomTeam = 
            gameDay
            |> GameDay.withTeam customTeam answers
        
        let totalAnswered = GameDay.totalAnswered gameDayWithCustomTeam gameDay.PackageSize customTeam
        
        PositiveNum.value gameDayWithCustomTeam.PackageSize = int totalAnswered
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Places don't intersect`` gameDay =
        
        let places = 
            gameDay 
            |> GameDay.teams
            |> Seq.map (GameDay.getPlace gameDay)
            |> Seq.distinct
            |> List.ofSeq
            
        let rec processPlace remainedPlaces = 
            let notIntersect one two =
                
                one.From < two.From && one.From < two.To ||
                two.From < one.From && two.From < one.To
            
            match remainedPlaces with
            | [] -> true
            | place :: others -> 
                
                let res = 
                    places
                    |> Seq.filter ((<>) place)
                    |> Seq.forall (notIntersect place)
                
                if res then processPlace others
                else false
        
        processPlace places