namespace GameDay.Tests

open Domain

open System
open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Utils



[<TestFixture>]
module GameDayExampleTests = 

    open TestUtils    

    let createTeam id name =
        {
            ID = id |> PositiveNum.ofInt |> Utils.okValueOrThrow;
            Name = name |> NoEmptyString.ofString |> Utils.okValueOrThrow
        }
        
    let firstTeam = createTeam 1 "Team1"
    let secondTeam = createTeam 2 "Team2"
    let thirdTeam = createTeam 3 "Team3"
    let fourthTeam = createTeam 4 "Team4"
    
    let withTeam' team answers gameDay = GameDay.withTeam team answers gameDay |> Utils.okValueOrThrow
    
    let createEmptyGameDay questionsCount =
        {
            Tournament = "Unit test tournament" |> NoEmptyString.ofString |> Utils.okValueOrThrow
            Name = DateTime.Now.ToString() |> NoEmptyString.ofString |> Utils.okValueOrThrow
            Answers = Map.empty
            PackageSize = questionsCount |> PositiveNum.ofInt |> Utils.okValueOrThrow
        }
    
    [<Test>]
    let ``LeadingTeams. N is greater than number of teams. Returns all teams``() = 
        
        let positiveNum3 = 
            PositiveNum.numOne |> PositiveNum.next |> PositiveNum.next
        

        let gameDay =
            createEmptyGameDay 3
            |> withTeam' firstTeam (Answers.ofBoolArray [|true; true; true;|])
            |> withTeam' secondTeam (Answers.ofBoolArray [|false; false; false;|])

        let top3Teams = GameDay.leadingTeams positiveNum3 gameDay
        let top3Count = top3Teams |> Seq.length
        
        top3Count |> should lessThan 3 

    [<Test>]
    let ``LeadingTeams. Two teams on secondPlace. Returns 3 teams on top2``() = 
        
        let positiveNum2 = 
            PositiveNum.numOne |> PositiveNum.next

        let gameDay = 
            createEmptyGameDay 5
            |> withTeam' firstTeam (Answers.ofBoolArray [|true;true;true;true;true;|])
            |> withTeam' secondTeam (Answers.ofBoolArray [|true;true;true;true;false;|])
            |> withTeam' thirdTeam (Answers.ofBoolArray [|true;true;true;true;false;|])
            |> withTeam' fourthTeam (Answers.ofBoolArray [|true;true;true;false;false;|])
    
        let top2Teams = GameDay.leadingTeams positiveNum2 gameDay
        let n = top2Teams |> Seq.length
        
        n |> should equal 3 

    [<Test>]
    let ``getAnswer. Team answered right on the first question. Returns Right``() = 
        
        let gameDay = 
            createEmptyGameDay 5
            |> withTeam' firstTeam (Answers.ofBoolArray [|true;false;false;true;true;|])
            
        let firstQuestion = PositiveNum.numOne
        let answerOnFirstQuestion = GameDay.getAnswer gameDay firstTeam firstQuestion
        
        answerOnFirstQuestion |> should equal Answer.Right 

    [<Test>]
    let ``getAnswer. Team answered wrong on the second question. Returns Wrong``() = 
        
        let gameDay = 
            createEmptyGameDay 5
            |> withTeam' firstTeam (Answers.ofBoolArray [|true;false;false;true;true;|])
            
        let secondQuestion = PositiveNum.numOne |> PositiveNum.next
        let answerOnSecondQuestion = GameDay.getAnswer gameDay firstTeam secondQuestion
        
        answerOnSecondQuestion |> should equal Answer.Wrong 

     
        
    [<Test>]
    let ``getDistanceFromFirstPlace. First team gives right answer on first question, second team doesn't. Distance is 1``() =
    
        let gameDay =
            createEmptyGameDay 1
            |> withTeam' firstTeam (Answers.ofBoolArray [|true;|])
            |> withTeam' secondTeam (Answers.ofBoolArray [|false;|])
            
                    
        let distance = GameDay.getGapFromTheFirstPlace gameDay secondTeam PositiveNum.numOne
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
            |> okValueOrThrow
            |> PositiveNum.next
            |> PositiveNum.createNaturalRange
        
        let usedIDs =     
            teams
            |> Seq.map (fun team -> team.ID)
        
        let newId = 
            possibleIds
            |> Seq.except usedIDs
            |> Seq.head
        
        {ID = newId; Name = okValueOrThrow <| NoEmptyString.ofString "Custom team"}

    let withTeam' team answers gameDay = GameDay.withTeam team answers gameDay |> Utils.okValueOrThrow
        
    let allWrongAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> false) 
        |> Answers.ofBoolArray
        
        
    let allRightAnswers questionsCount =
        
        Array.init questionsCount (fun _ -> true) 
        |> Answers.ofBoolArray

    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Teams that gives maximum right answers are on the first place`` gameDay =
        
        let teamWithMaximumRightAnswers =
            gameDay
            |> GameDay.teams
            |> Seq.maxBy (GameDay.totalAnswered gameDay gameDay.PackageSize)
            
        let leaderPlace =
            teamWithMaximumRightAnswers
            |> GameDay.getPlace gameDay 
        
        leaderPlace.From = PositiveNum.numOne
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Teams that gives minimum right answers are on the last place`` gameDay =
        
        let teamWithMinimumAnswers =
            gameDay
            |> GameDay.teams
            |> Seq.minBy (GameDay.totalAnswered gameDay gameDay.PackageSize)
            
        let lastPlace = gameDay |> GameDay.teams |> Seq.length |> PositiveNum.ofInt |> okValueOrThrow
            
        let place = GameDay.getPlace gameDay teamWithMinimumAnswers
        printfn "teams %d place %A" (GameDay.teams gameDay |> Seq.length)  place
        place.To = lastPlace
        
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Leading team has 0 gap from the first place`` gameDay =
        
        let leader =
            gameDay
            |> GameDay.leadingTeams PositiveNum.numOne
            |> Seq.head
        
        let gap = GameDay.getGapFromTheFirstPlace gameDay leader gameDay.PackageSize
        gap = 0<RightAnswer>
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Teams must be unique in the game day`` gameDay =
        
        let existingTeam = gameDay |> GameDay.teams |> Seq.head
        
        let actual = 
            gameDay
            |> GameDay.withTeam existingTeam (gameDay.Answers |> Map.find existingTeam)
            
        match actual with
        | Ok data -> false
        | Error message -> String.containsSubstring "is already added" message 
            
        
    [<Property(QuietOnSuccess = true)>]
    let ``GameDay property. Questions package size are equal to questions array length`` num1 num2 =
        
        let precondition num1 num2 = num1 > 0 && num2 > 0 && num1 <> num2
        
        let property questionsCount answersLength =
            
            let gameDay =  
                {
                    Tournament = "Generated tournament" |> NoEmptyString.ofString |> okValueOrThrow
                    Name = DateTime.Now.ToString() |> NoEmptyString.ofString |> okValueOrThrow
                    Answers = Map.empty
                    PackageSize = questionsCount |> PositiveNum.ofInt |> okValueOrThrow
                }
                
            let customTeam = {ID = PositiveNum.numOne; Name = okValueOrThrow <| NoEmptyString.ofString "Test team"}
            let answers = Array.init answersLength (fun _ -> true) |> Answers.ofBoolArray
            
            match gameDay |> GameDay.withTeam customTeam answers with
            | Ok _ -> false
            | Error message -> String.containsSubstring "Questions count" message
                
        (precondition num1 num2) ==> lazy(property num1 num2)
        
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. All team answers are wrong. Total answered is 0`` gameDay =
        
        let loserTeam = gameDay |> GameDay.teams |> createNewTeam
         
        
        let gameDayWithLoserTeam =
            
            let answers = allWrongAnswers <| PositiveNum.value gameDay.PackageSize
            
            gameDay
            |> withTeam' loserTeam answers
        
        let totalAnswered = GameDay.totalAnswered gameDayWithLoserTeam gameDayWithLoserTeam.PackageSize loserTeam
        
        totalAnswered = 0<RightAnswer>
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. All team answers are right. Total answered questions is equal to questions count`` gameDay =
        
        let customTeam = gameDay |> GameDay.teams |> createNewTeam
        let answers = allRightAnswers (PositiveNum.value gameDay.PackageSize) 
        
        let gameDayWithCustomTeam = 
            gameDay
            |> withTeam' customTeam answers
        
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
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Places hasn't gaps`` gameDay =
        
        let places = 
            gameDay 
            |> GameDay.teams
            |> Seq.map (GameDay.getPlace gameDay)
            |> Seq.distinct
            |> Seq.sort
            
        let totalRange = 
            
            places
            |> Seq.reduce (fun p1 p2 -> if PositiveNum.next p1.To = p2.From then {From = p1.From; To = p2.To} else p1)
            
        let teamsCount =
            gameDay |> GameDay.teams |> Seq.length
            
        totalRange =
            {
                From = PositiveNum.numOne
                To = teamsCount |> PositiveNum.ofInt |> okValueOrThrow 
            }
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Team from the first place is always head of list`` gameDay =
        
        let (_, _, place) = 
            gameDay
            |> GameDay.getRating
            |> List.head
            
        place.From = PositiveNum.numOne
        
    [<Property(QuietOnSuccess = true, MaxTest = 10, Arbitrary = [|typeof<GameDayType>|])>]
    let ``GameDay property. Game day rating is equal to rating with difficult questions with threshold same as teams count`` gameDay =
        
        let gameDayRating =
            gameDay
            |> GameDay.getRating
            
        let threshold = gameDay |> GameDay.teams |> Seq.length |> Converter.rightAnswerFromInt
            
        let filteredRating =
            gameDay
            |> GameDay.getRatingOnDifficultQuestions threshold
            
        gameDayRating = filteredRating