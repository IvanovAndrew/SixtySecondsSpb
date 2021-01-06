namespace SixtySeconds.Infrastructure

open Shared
open Shared.Models

module DomainToModelMapping =
    
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.Domain
    
    
    let teamToModel (team : Team) : TeamModel =
        {
            Id = team.ID.Value
            Name = team.Name.Value
        }
        
    let placeToModel (place : Place) : PlaceModel =
        {
            From = place.From.Value
            To = place.To.Value
        }
        
    let placeInfoToModel (placeInfo : PlaceInfo) : PlaceInfoModel =
        {
            Place = placeToModel placeInfo.Place
            Question = placeInfo.Question.Value
        }
        
    let tournamentToModel (tournament : Tournament) : TournamentModel =
        {
            City = tournament.City.Value
            League = tournament.League.Value
            Season = tournament.Season.Value
        }
        
    let answerOnQuestionToModel (answerOnQuestion: AnswerOnQuestion) : AnswerOnQuestionModel =
        {
            Number = answerOnQuestion.Number.Value
            Answer = answerOnQuestion.Answer |> Answer.isRight
        }
        
    let strikeToModel (strike : StrikeInfo) : Shared.StrikeInfo =
        
        {
            Type =
                match strike.Type with
                | StrikeType.Best -> Shared.StrikeType.Best
                | StrikeType.Worst -> Shared.StrikeType.Worst
            Count = strike.Count |> Option.map (fun v -> v.Value)
        }
        
    let answersToModel (answers: Answers) : AnswersModel =
        answers
        |> Answers.toList
        |> List.map answerOnQuestionToModel
        
    let ratingPoints f =
        match f.Point with 
        | Missed -> 0m
        | Played p -> decimal p
        
    let gamePointsToModel (g : GamedayPoint) =
        {
            Date = g.Date
            Points = match g.Point with Played p -> decimal p | Missed -> 0m
        }
        
    let gameDayToModel (gameDay : GameDay) : GameDayModel =
        {
            Tournament = tournamentToModel gameDay.Tournament
            Name = gameDay.Name.Value
            Answers = gameDay.Answers |> Map.toSeq |> Seq.map (fun (team, answers) -> teamToModel team, answersToModel answers) |> Map.ofSeq
            PackageSize = gameDay.PackageSize.Value
        }
        
    let seasonResultToModel (seasonResult : SeasonResults) : SeasonResultModel =
        
        seasonResult
        |> Map.toSeq
        |> Seq.map (fun (team, results) -> teamToModel team, results |> List.map gamePointsToModel)
        |> Map.ofSeq
        
    let seasonTableToModel (seasonTable : SeasonTable) : SeasonTableModel =
        {
            Results = seasonResultToModel seasonTable.Results 
            Table = seasonTable.Table |> Seq.map (fun (team, point, place) -> teamToModel team, decimal point, placeToModel place) |> List.ofSeq
            GamesCount = seasonTable.GamesCount.Value
        }
        
    let teamPerformanceToModel (teamPerformance : TeamPerformance) : TeamPerformanceModel =
        
        {
            Team = teamPerformance.Team |> teamToModel
            BestPlace = teamPerformance.BestPlace |> placeInfoToModel
            WorstPlace = teamPerformance.WorstPlace |> placeInfoToModel
            BestStrike = teamPerformance.BestStrike |> strikeToModel 
            WorstStrike = teamPerformance.WorstStrike |> strikeToModel
            DifficultAnsweredQuestion = teamPerformance.DifficultAnsweredQuestion.Value
            DifficultAnsweredQuestionCount = teamPerformance.DifficultAnsweredQuestionCount
            SimplestWrongAnsweredQuestion = teamPerformance.SimplestWrongAnsweredQuestion.Value
            SimplestWrongAnsweredQuestionCount = teamPerformance.SimplestWrongAnsweredQuestionCount
        }