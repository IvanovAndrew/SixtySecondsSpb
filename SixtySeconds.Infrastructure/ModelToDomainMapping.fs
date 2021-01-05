namespace SixtySeconds.Infrastructure

open SixtySeconds.Actions

module ModelToDomainMapping =
    
    open Shared.Models
    open SixtySeconds.Domain
    open SixtySeconds.Common.CommonTypes
    
    let modelToTeam (team : TeamModel) : Team =
        {
            ID = PositiveNum.ofConst team.Id
            Name = NoEmptyString.ofConstString team.Name
        }
        
    let modelToTournament (model : TournamentModel) : Tournament =
        {
            City = NoEmptyString.ofConstString model.City
            League = NoEmptyString.ofConstString model.League
            Season = NoEmptyString.ofConstString model.Season
        }
        
    
    
    let modelToAnswerOnQuestion (model: AnswerOnQuestionModel) : AnswerOnQuestion =
        {
            Number = PositiveNum.ofConst model.Number
            Answer = Answer.ofBool model.Answer
        }
        
    let modelToAnswers (answers : AnswersModel) : Answers =
        answers
        |> List.map modelToAnswerOnQuestion
        |> Answers.ofAnswersList
        
    let modelToGameDay (model : GameDayModel) : GameDay =
        {
            Tournament = modelToTournament model.Tournament
            Name = NoEmptyString.ofConstString model.Name
            Answers = model.Answers |> Map.toSeq |> Seq.map (fun (team, answers) -> modelToTeam team, modelToAnswers answers) |> Map.ofSeq
            PackageSize = PositiveNum.ofConst model.PackageSize 
        }
        
    let modelToGamePoints (model : GamedayPointsModel) : GamedayPoint =
        {
            Date = model.Date
            Point = if model.Points > 0m then model.Points |> Converter.pointFromDecimal |> Played else Missed 
        }
        
    
    let modelToSeasonRating (model : Map<TeamModel, GamedayPointsModel list>) : SeasonResults =
        
        model
        |> Map.toSeq
        |> Seq.map (fun (t, v) -> modelToTeam t, v |> List.map modelToGamePoints)
        |> Map.ofSeq
        
    let modelToSeasonRatingOptions (model : RatingFilterModel) : SeasonResultFilter =
        {
            GamesToCount = PositiveNum.ofConst model.GamesToCount
            FinalDate =
                match model.FinalDate with
                | FinalDateModel.PlayedAlready p -> FinalDate.AlreadyPlayed p
                | FinalDateModel.NotPlayedYet -> FinalDate.NotPlayedYet
            RatingOption =
                match model.RatingOption with
                | Shared.Models.RatingOption.FinalGameCounts -> RatingOption.FinalGameCounts
                | Shared.Models.RatingOption.FinalGameDoesntCount -> RatingOption.FinalGameDoesntCount
        }