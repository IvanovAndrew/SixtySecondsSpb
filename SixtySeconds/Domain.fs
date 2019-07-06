module Domain

open Utils
open System

type Team = 
    {
        ID : PositiveNum
        Name : NoEmptyString
    }

type Place = 
    {
        From : PositiveNum
        To : PositiveNum
    }

type Answer = 
    | Right 
    | Wrong

module Answer = 
        
    let ofBool isRight = if isRight then Right else Wrong
        
type Answers = private Answers of Answer array
    
module Answers = 
        
    let ofArray arr = Answers arr
    let ofSeq seq = seq |> Array.ofSeq |> ofArray
        
    let getAnswer questionNumber (Answers a) = 
        let index = 
            questionNumber
            |> PositiveNum.value 
            |> ((+) -1)
        a.[index]

    let count (Answers a) = a |> Array.length |> PositiveNum.ofInt

type GameDay = 
    {
        Day : DateTime
        Answers : Map<Team, Answers>
        QuestionsCount : PositiveNum
    }

module GameDay = 
        
    let ofMap day answers = 
        {
            Day = day; 
            Answers = answers; 
            QuestionsCount = answers |> Map.count |> PositiveNum.ofInt
        }
        
    let teams gameDay = 
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map fst
        
    /// Did team A answer question Q
    let getAnswer gameDay team questionNumber = 
            
        gameDay.Answers 
        |> Map.find team
        |> Answers.getAnswer questionNumber

    /// How many correct answers did the team give as of question Q
    let totalAnswered gameDay team questionNumber = 
        
        let getAnswer' = getAnswer gameDay team

        let rec getTotalAnswered acc question = 
            
            let newAcc = 
                match getAnswer' question with 
                | Right -> acc + 1
                | Wrong -> acc

            if question = PositiveNum.numOne then newAcc
            else 
                getTotalAnswered newAcc <| PositiveNum.previous question
        
        getTotalAnswered 0 questionNumber

    /// Отставание команды A от лидера по состоянию на вопрос Q
    let getDistanceFromFirstPlace gameDay myTeam questionNumber = 
            
        let teamAnsweredOn = totalAnswered gameDay myTeam questionNumber

        let leaderAnsweredOn = 
            gameDay
            |> teams
            |> Seq.map (fun t -> totalAnswered gameDay t questionNumber)
            |> Seq.max
        
        teamAnsweredOn - leaderAnsweredOn

    /// Team A is behind the leader as of question Q
    let getPlaceAfterQuestion gameDay myTeam questionNumber = 
            
        let threshold = totalAnswered gameDay myTeam questionNumber

        let processTeam (placeUp, placeDown) team = 
            
            if team = myTeam then placeUp, placeDown
            else 
                let answered = totalAnswered gameDay team questionNumber
                
                if answered > threshold then placeUp |> PositiveNum.next, placeDown |> PositiveNum.next
                elif answered = threshold then placeUp, placeDown |> PositiveNum.next
                else placeUp, placeDown


        let placeUp, placeDown = 
            gameDay
            |> teams
            |> Seq.fold processTeam (PositiveNum.numOne, PositiveNum.numOne)
            
        {
            From = placeUp
            To = placeDown
        }

    /// Number of teams that correctly answered on question 
    let rightAnswersOnQuestion gameDay question = 
        gameDay
        |> teams
        |> Seq.map (fun t -> getAnswer gameDay t question)
        |> Seq.sumBy (function Right -> 1 | _ -> 0)

    
    let getTopNTeams gameDay n = 
        
        gameDay
        |> teams
        |> Seq.groupBy (fun t -> totalAnswered gameDay t gameDay.QuestionsCount)
        |> Seq.sortByDescending fst
        |> Seq.fold (fun res group -> 
                            if n > Seq.length res 
                            then group |> snd |> Seq.append res 
                            else res) Seq.empty

    /// 
    let getWinnerTeam gameDay = getTopNTeams gameDay 1


    /// 
    let getDifficultQuestions gameDay = 
        
        let threshold = 
            let teamsCount = gameDay |> teams |> Seq.length
            teamsCount / 3

        let isDifficult question = 
            
            let rightAnswers = rightAnswersOnQuestion gameDay question
            rightAnswers <= threshold


        gameDay.QuestionsCount
        |> PositiveNum.createNaturalRange
        |> Seq.filter isDifficult

    let getRatingOnDifficultQuestions gameDay = 
        
        let questions = getDifficultQuestions gameDay

        let answeredOnDifficultQuestions team = 
            
            questions
            |> Seq.map (getAnswer gameDay team)
            |> Seq.sumBy (function Right -> 1 | _ -> 0)

        gameDay
        |> teams
        |> Seq.map (fun t -> t, answeredOnDifficultQuestions t)
        |> Seq.sortByDescending (fun (_, a) -> a)

type SeasonTable = 
    {
        Results : Map<Team, decimal seq>
        Table : (Team * decimal) seq
        GamesCount : PositiveNum
    }

module SeasonTable = 
    
    let ofSeq data = 
        
        {
            Table = data |> Seq.map (fun (t, r) -> t, r |> Seq.sum) |> Seq.sortByDescending snd
            Results = data |> Map.ofSeq
            GamesCount = data |> Seq.map (snd >> Seq.length) |> Seq.max |> PositiveNum.ofInt
        }


    let topNResult resultsToCount seasonTable = 
            
        let gamesToCount = 
            if resultsToCount > seasonTable.GamesCount
            then seasonTable.GamesCount
            else resultsToCount

        seasonTable.Results
        |> Map.toSeq
        |> Seq.map (fun (team, results) -> 
                                
                        let length = results |> Seq.length |> PositiveNum.ofInt

                        let topResults = 
                            if length < gamesToCount 
                            then results
                            else results |> Seq.sortByDescending id |> Seq.take (gamesToCount |> PositiveNum.value)
                                
                        team, topResults |> Seq.sum
                        )
        |> Seq.sortByDescending snd