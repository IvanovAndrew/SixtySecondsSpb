namespace SixtySeconds.Infrastructure

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