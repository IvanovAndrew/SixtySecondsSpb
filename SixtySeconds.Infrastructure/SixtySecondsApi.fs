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
        
    let toDto f result =
        async {
            let! res = result
            return
                res
                |> Result.map f
        }
        
    
    let gameDayToDto gdResult =
        async {
            let! result = gdResult  
            
            return result |> Result.map DomainToModelMapping.gameDayToModel 
        }
        
    let seasonResultToDto totalResult =
        async {
            let! result = totalResult
            
            return result |> Result.map DomainToModelMapping.seasonResultToModel
        }
    
    let tableToDto (table : Async<Result<Domain.SeasonRating, _>>) =
        
        let mapLine (team, rating, place) =
            DomainToModelMapping.teamToModel team,
            Converter.toDecimal rating,
            DomainToModelMapping.placeToModel place
        
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

    let teamPerformance (gameDay, team) =
        (ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySecondsWorkflow.teamPerformance >> SixtySecondsProgramInterpreter.interpretSimple >> (toDto DomainToModelMapping.teamPerformanceToModel) >> mapError)
        
    let parseTotal arg =
        arg |> (SixtySecondsWorkflow.parseTotal >> SixtySecondsProgramInterpreter.interpret >> seasonResultToDto >> mapError)
        
    let parseGameDay arg =
        arg |> (SixtySecondsWorkflow.parseGameDay >> SixtySecondsProgramInterpreter.interpret >> gameDayToDto >> mapError)
        
    let showChart (chartType, gameDay) =
        (chartType, ModelToDomainMapping.modelToGameDay gameDay) |> SixtySeconds.Services.ServiceActions.showChart
        
    let writeToSpreadsheet (sheetOptions, gameDay, team) =
        (sheetOptions, ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySeconds.Services.ServiceActions.writeToSpreadsheet >> mapError)