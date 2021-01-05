namespace SixtySeconds.Infrastructure

open System
open SixtySeconds.Actions

module SixtySecondsApi =
    
    open SixtySeconds
    open Shared
    
    
    let mapError result =
        async {
            let! res = result
            return res |> Result.mapError SixtySeconds.Common.ErrorMessages.errorToString
        }
    
    let gameDayToDto gdResult =
        async {
            let! result = gdResult  
            
            return result |> Result.map Models.gameDayToModel 
        }
        
    let seasonResultToDto totalResult =
        async {
            let! result = totalResult
            
            return result |> Result.map Models.seasonResultToModel
        }
    
    let tableToDto (table : Async<Result<Domain.SeasonRating, _>>) =
        
        let mapLine (team, rating, place) =
            Models.teamToModel team,
            Converter.toDecimal rating,
            Models.placeToModel place
        
        async {
            let! res = table
            
            return
                res
                |> Result.map (fun sr -> sr |> List.map mapLine)
        }
        
    
    
    let gameDayRating arg =
           arg |> (SixtySecondsWorkflow.gameDayRating >> SixtySecondsProgramInterpreter.interpretSimple >> mapError)
                   
                   
    let filterTotalTable (options, results) =
        (ModelToDomainMapping.modelToSeasonRatingOptions options, ModelToDomainMapping.modelToSeasonRating results)
        |> (SixtySecondsWorkflow.filterSeasonResults >> SixtySecondsProgramInterpreter.interpretSimple >> tableToDto >> mapError)

    let teamBestPlace arg =
        arg |> (SixtySecondsWorkflow.teamBestPlace >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamWorstPlace arg =
        arg |> (SixtySecondsWorkflow.teamWorstPlace >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamBestStrike arg =
        arg |> (SixtySecondsWorkflow.teamBestStrike >> SixtySecondsProgramInterpreter.interpretSimple)
    
    let teamWorstStrike arg =
        arg |> (SixtySecondsWorkflow.teamWorstStrike >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamDifficultAnsweredQuestion arg =
        arg |> (SixtySecondsWorkflow.teamDifficultAnsweredQuestion >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamDifficultAnsweredQuestionCount arg =
        arg |> (SixtySecondsWorkflow.teamDifficultAnsweredQuestionCount >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamSimpleWrongAnsweredQuestion arg =
        arg |> (SixtySecondsWorkflow.teamSimpleWrongAnsweredQuestion >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamSimpleWrongAnsweredQuestionCount arg =
        arg |> (SixtySecondsWorkflow.teamSimpleWrongAnsweredQuestionCount >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamPerformance arg =
        arg |> (SixtySecondsWorkflow.teamPerformance >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let parseTotal arg =
        arg |> (SixtySecondsWorkflow.parseTotal >> SixtySecondsProgramInterpreter.interpret >> seasonResultToDto >> mapError)
        
    let parseGameDay arg =
        arg |> (SixtySecondsWorkflow.parseGameDay >> SixtySecondsProgramInterpreter.interpret >> gameDayToDto >> mapError)
        
    let showChart (chartType, gameDay) =
        (chartType, ModelToDomainMapping.modelToGameDay gameDay) |> SixtySeconds.Services.ServiceActions.showChart
        
    let writeToSpreadsheet (sheetOptions, gameDay, team) =
        (sheetOptions, ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySeconds.Services.ServiceActions.writeToSpreadsheet >> mapError)