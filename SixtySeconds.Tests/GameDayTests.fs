namespace GameDay.Tests

open NUnit.Framework

[<TestFixture>]
module GameDayTests = 
    open Domain
    open Utils

    open System

    let createTeam id name = {ID = id |> PositiveNum.ofInt; Name = name |> NoEmptyString.ofString}

    let createGameDay teams = 
        
        let createAnswers arr = 
            arr 
            |> Seq.map Answer.ofBool
            |> Answers.ofSeq

        let questionsCount = teams |> Seq.head |> (fun (_, _, l) -> l) |> Seq.length

        let createTeamInfo (id, name, answers) = 
            let team = createTeam id name
            team, createAnswers answers

        {
            Day = DateTime.Now
            Answers = teams |> Seq.map createTeamInfo |> Map.ofSeq
            QuestionsCount = PositiveNum.ofInt questionsCount
        }

    
    [<Test>]
    let ``GetTopN. N is greater than number of teams. Returns all teams``() = 
        
        let gameDay = createGameDay 
                            [
                                (1, "Team1", [true; true; true;]); 
                                (2, "Team2", [false; false; false;]);
                            ] 

        let top3Teams = GameDay.getTopNTeams gameDay 3
        let top3Count = top3Teams |> Seq.length
        
        top3Count < 3
        |> NUnitAssert.isTrue

    [<Test>]
    let ``GetTopN. Two teams on secondPlace. Returns 3 teams on top2``() = 
        
        let gameDay = 
            createGameDay 
                    [
                        (1, "Team1", [true;true;true;true;true;]);
                        (2, "Team2", [true;true;true;true;false;]);
                        (3, "Team3", [true;true;true;true;false;]);
                        (4, "Team4", [true;true;true;false;false;]);
                    ]
            
    
        let top2Teams = GameDay.getTopNTeams gameDay 2
        let n = top2Teams |> Seq.length
        
        NUnitAssert.areEqual n 3

    [<Test>]
    let ``getAnswer. Team answered right on the first question. Returns Right``() = 
        
        let team = createTeam 1 "Team1"

        let gameDay = 
            createGameDay 
                    [
                        (1, "Team1", [true;false;false;true;true;]);
                    ]
            
        let firstQuestion = 1 |> PositiveNum.ofInt
        let answerOnFirstQuestion = GameDay.getAnswer gameDay team firstQuestion
        
        NUnitAssert.areEqual Answer.Right answerOnFirstQuestion

    [<Test>]
    let ``getAnswer. Team answered wrong on the second question. Returns Wrong``() = 
        
        let team = createTeam 1 "Team1"

        let gameDay = 
            createGameDay 
                    [
                        (1, "Team1", [true;false;false;true;true;]);
                    ]
            
        let secondQuestion = 2 |> PositiveNum.ofInt
        let answerOnSecondQuestion = GameDay.getAnswer gameDay team secondQuestion
        
        NUnitAssert.areEqual Answer.Wrong answerOnSecondQuestion

    [<Test>]
    let ``getAnswer. Questions count 5. Throws exception on 6``() = 
        
        let team = createTeam 1 "Team1"

        let gameDay = 
            createGameDay 
                    [
                        (1, "Team1", [true;false;false;true;true;]);
                    ]
            
        let sixthQuestion = 6 |> PositiveNum.ofInt
        
        try
            let answerOnSixthQuestion = GameDay.getAnswer gameDay team sixthQuestion
            NUnitAssert.Fail()
        with 
            | _ -> NUnitAssert.Pass()
        
        