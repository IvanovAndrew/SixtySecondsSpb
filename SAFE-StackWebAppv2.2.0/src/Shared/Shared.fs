namespace Shared

open System
open Models
open Shared

type RatingType =
    | All
    | Threshold of int

type TeamRatingPosition<'a> = TeamModel * 'a * PlaceModel
type GameDayRating = TeamRatingPosition<int> list
type SeasonRating = TeamRatingPosition<decimal> list

type StrikeType =
        | Best
        | Worst

type StrikeInfo =
        {
            Type : StrikeType
         //   From : QuestionNumber
         //   To : QuestionNumber
            Count : int option
        }
        

//type PlaceInfo = {Place : Place; Question : int}

type TeamPerformanceModel =
    {
        Team : TeamModel
        BestPlace : PlaceInfoModel
        WorstPlace : PlaceInfoModel
        BestStrike : StrikeInfo
        WorstStrike : StrikeInfo
        DifficultAnsweredQuestion : int
        DifficultAnsweredQuestionCount : int
        SimplestWrongAnsweredQuestion : int
        SimplestWrongAnsweredQuestionCount : int
    }

type SeasonTable =
    {
        Results : Map<TeamModel, decimal seq>
        Table : SeasonRating
        GamesCount : int
    }
    
module TeamPerformanceModel =
    let defaultTeamPerformanceModel team =
        {
            Team = team
            BestPlace = {Place = {From = 1; To = 1}; Question = 1}
            WorstPlace = {Place = {From = 1; To = 1}; Question = 1}
            BestStrike = { Type = StrikeType.Best; Count = None }
            WorstStrike = { Type = StrikeType.Worst; Count = None }
            DifficultAnsweredQuestion = -1
            DifficultAnsweredQuestionCount = -1
            SimplestWrongAnsweredQuestion = -1
            SimplestWrongAnsweredQuestionCount = -1
        }

module Place =

    let toString (place : PlaceModel) =
        if place.From = place.To then string place.From
        else sprintf "%d-%d" place.From place.To

module GameDay =  /// Team played at game day
    let teams (gameDay : GameDayModel) =
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map fst

    let answers gameDay =
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map snd

    let allQuestions gameDay = [1..gameDay.PackageSize]

    let getAnswer questionNumber answers =

        let answerOnQuestion =
            answers
            |> Seq.find (fun item -> item.Number = questionNumber)
        answerOnQuestion.Answer

    /// Number of teams that correctly answered on question
    let rightAnswers gameDay question =
        gameDay
        |> answers
        |> Seq.map (getAnswer question)
        |> Seq.filter id
        |> Seq.length

    let toTriple gameDay =
        (
            (gameDay.Tournament.City, gameDay.Tournament.League, gameDay.Tournament.Season),
            gameDay.Answers |> Map.toList |> List.map (fun (team, answers) -> (team.Id, team.Name), ())
        )

type ShowChartsInput =
    | CustomTeamsOnly of TeamModel list
    | BestTeamsOnly of int
    | CustomTeamsAndBestTeams of teams : TeamModel list * bestTeams : int

type ChartType =
    | Answers of ShowChartsInput
    | Places of ShowChartsInput
    
module SeasonResult =
    
    let gamesAmount seasonResultModel =
        seasonResultModel
        |> Map.toList
        |> List.map (snd >> List.length)
        |> List.max
        
    let gameDates seasonResultModel =
        seasonResultModel
        |> Map.toList
        |> List.map snd
        |> List.collect(fun results -> results |> List.map (fun res -> res.Date))
        |> List.distinct
        |> List.sort
        

module Rating =

    let teamPlaces teamToRating teams : Map<TeamModel, PlaceModel> =

        let proc (acc, fromPlace) group =

            let place =
                let toPlace =

                    let groupLength = group |> Seq.length
                    if groupLength <= 1 then fromPlace
                    else
                        (groupLength - 1) + fromPlace

                {From = fromPlace; To = toPlace}


            let newGroup =
                group
                |> Seq.map (fun (team, rating) -> team, place)
                |> Seq.append acc

            newGroup, place.To + 1

        teams
        |> Seq.map (fun team -> team, teamToRating team)
        |> Seq.groupBy (fun (_, rating) -> rating)
        |> Seq.sortByDescending (fun (key, _) -> key)
        |> Seq.map snd
        |> Seq.fold proc (Seq.empty, 1)
        |> fst
        |> Map.ofSeq

    let ofGameDay gameDay : GameDayRating =
        
        // TODO memoize
        let teamToRating team = gameDay.Answers.[team] |> Seq.filter (fun a -> a.Answer) |> Seq.length
        let teamToPlace = 
            gameDay
            |> GameDay.teams
            |> teamPlaces teamToRating
        
        gameDay
        |> GameDay.teams
        |> Seq.map (fun team -> team, teamToRating team, teamToPlace.[team])
        |> Seq.sortByDescending (fun (_, rating, _) -> rating)
        |> List.ofSeq

    
//    let ofGameDayWithFilter questions gameDay : GameDayRating =
//
//        let answeredOnDifficultQuestions team =
//
//            gameDay.Answers
//            |> Map.find team
//            |> Answers.filterByQuestionNumber questions
//            |> Answers.sumRightAnswers
//
//        gameDay
//        |> GameDay.teams
//        |> Seq.map (fun t -> t, answeredOnDifficultQuestions t)
//        |> ofSeq

    let leadingTeams topN rating =

        rating
        |> Seq.takeWhile (fun (_, _, place) -> place.From <= topN)
        |> Seq.map (fun (team, _, _) -> team)
        
module Utils =
    
    let parseDate (str : string) =
        match str.Split('.', StringSplitOptions.None) with
        | [| day; month; year |] ->
            
            let tryParseInt (s : string) =
                match Int32.TryParse(s) with
                | (true, value) -> Ok value
                | _ -> Error <| sprintf "Couldn't parse %s as int" s
              
            let date = tryParseInt day
            let month = tryParseInt month
            let year = tryParseInt year
            
            match date, month, year with
            | Ok d, Ok m, Ok y -> Ok <| DateTime((if y < 100 then 2000 + y else y), m, d)
            | _ -> Error <| sprintf "Couldn't parse %s as date" str
        | _ -> Error <| sprintf "Couldn't parse %s as date" str
        
    let tryParseDateTime (str : string) =
        match DateTime.TryParse(str) with
        | true, date -> Some date
        | false, _ -> None