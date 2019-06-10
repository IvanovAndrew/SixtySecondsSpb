namespace GameDay.Tests

open NUnit.Framework

[<TestFixture>]
module GameDayTests = 
    open Domain
    open Utils

    open System

    let createAnswers arr = 
        arr 
        |> Seq.map Answer.ofBool
        |> Answers.ofSeq
    
    [<Test>]
    let ``GetTopN. N is greater than number of teams. Returns all teams``() = 
        
        let createAnswers n = 
            
            let getRandomBool i = 
                let r = new Random()
                r.Next(0, 1) > 0

                
            getRandomBool
            |> List.init n
            |> createAnswers

        let questionsCount = 5

        let gameDay = 
            {
                Day = DateTime.Now
                Answers = 
                    [
                        {ID = 1 |> PositiveNum.ofInt; Name = "Team1" |> NoEmptyString.ofString}, createAnswers questionsCount;
                        {ID = 2 |> PositiveNum.ofInt; Name = "Team2" |> NoEmptyString.ofString}, createAnswers questionsCount;
                    ]
                    |> Map.ofList
                QuestionsCount = PositiveNum.ofInt questionsCount
            }
    
        let top3Teams = GameDay.getTopNTeams gameDay 3
        let n = top3Teams |> Seq.length
        
        n < 3
        |> NUnitAssert.isTrue

    [<Test>]
    let ``GetTopN. Two teams on secondPlace. Returns 3 teams on top2``() = 
        
        let questionsCount = 5

        let gameDay = 
            {
                Day = DateTime.Now
                Answers = 
                    [
                        {ID = 1 |> PositiveNum.ofInt; Name = "Team1" |> NoEmptyString.ofString}, createAnswers [true;true;true;true;true;];
                        {ID = 2 |> PositiveNum.ofInt; Name = "Team2" |> NoEmptyString.ofString}, createAnswers [true;true;true;true;false;];
                        {ID = 3 |> PositiveNum.ofInt; Name = "Team3" |> NoEmptyString.ofString}, createAnswers [true;true;true;true;false;];
                        {ID = 4 |> PositiveNum.ofInt; Name = "Team4" |> NoEmptyString.ofString}, createAnswers [true;true;true;false;false;];
                    ]
                    |> Map.ofList
                QuestionsCount = PositiveNum.ofInt questionsCount
            }
    
        let top2Teams = GameDay.getTopNTeams gameDay 2
        let n = top2Teams |> Seq.length
        
        NUnitAssert.areEqual n 3