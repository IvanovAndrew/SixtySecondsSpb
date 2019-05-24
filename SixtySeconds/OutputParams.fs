module OutputParams

open Domain
open Utils

type OutputParams = 
    {
        Team : Team 
        TeamAnswers : Answer list
        RightAnswersOn : int list
        Places : Place list
        Distance : int list
    }

let outputParams myTeam gameDay = 
            
    let allQuestions = 
        PositiveNum.createNaturalRange gameDay.QuestionsCount

    let teamAnswered = 
        allQuestions
        |> List.map (GameDay.getAnswer gameDay myTeam)

    let rightAnsweredOn = 
        allQuestions
        |> List.map (GameDay.rightAnswersOnQuestion gameDay)

    let places = 
        allQuestions
        |> List.map (GameDay.getPlaceAfterQuestion gameDay myTeam)

    let distance = 
        allQuestions 
        |> List.map (GameDay.getDistanceFromFirstPlace gameDay myTeam)

    {
        Team = myTeam
        TeamAnswers = teamAnswered
        RightAnswersOn = rightAnsweredOn
        Places = places
        Distance = distance
    }