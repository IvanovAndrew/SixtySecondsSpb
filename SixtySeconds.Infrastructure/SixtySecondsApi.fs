namespace SixtySeconds.Infrastructure

open System
open SixtySeconds.Actions

module SixtySecondsApi =
    
    open SixtySeconds
    open Shared
    
    
    let private mapError result =
        async {
            let! res = result
            return res |> Result.mapError SixtySeconds.Common.ErrorMessages.errorToString
        }
        
    let private toDto f result =
        async {
            let! res = result
            return
                res
                |> Result.map f
        }
        
    
    let private gameDayToDto gdResult =
        async {
            let! result = gdResult  
            
            return result |> Result.map DomainToModelMapping.gameDayToModel 
        }
        
    let private seasonResultsToDto totalResult =
        async {
            let! result = totalResult
            
            return result |> Result.map (fun (secondsTable, matrixTable) -> secondsTable |> DomainToModelMapping.seasonResultToModel, matrixTable |> DomainToModelMapping.seasonResultToModel)
        }
    
    let private seasonTableToDto (table : Async<Result<Domain.SeasonRating, _>>) =
        
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
        
    let private gameDayTableToDto (table : Async<Result<Domain.GameDayRating, _>>) =
        
        let mapLine (team, rating, place) =
            DomainToModelMapping.teamToModel team,
            rating |> Converter.toInt |> decimal,
            DomainToModelMapping.placeToModel place
        
        async {
            let! res = table
            
            return
                res
                |> Result.map (fun sr -> sr |> List.map mapLine)
        }
        
    
    
    let gameDayRating (filter, gameDay) =
           (ModelToDomainMapping.modelToGamedayFilter filter, ModelToDomainMapping.modelToGameDay gameDay)
           |> (SixtySecondsWorkflow.gameDayRating >> SixtySecondsProgramInterpreter.interpretSimple >> gameDayTableToDto >> mapError)
                   
                   
    let filterTotalTable (options, results) =
        (ModelToDomainMapping.modelToSeasonRatingOptions options, ModelToDomainMapping.modelToSeasonRating results)
        |> (SixtySecondsWorkflow.filterSeasonResults >> SixtySecondsProgramInterpreter.interpretSimple >> seasonTableToDto >> mapError)

    let teamPerformance (gameDay, team) =
        (ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySecondsWorkflow.teamPerformance >> SixtySecondsProgramInterpreter.interpretSimple >> (toDto DomainToModelMapping.teamPerformanceToModel) >> mapError)
        
    let parseTotal arg =
        arg |> (SixtySecondsWorkflow.parseTotal >> SixtySecondsProgramInterpreter.interpret >> seasonResultsToDto >> mapError)
        
    let parseGameDay arg =
        arg |> (SixtySecondsWorkflow.parseGameDay >> SixtySecondsProgramInterpreter.interpret >> gameDayToDto >> mapError)
        
    let showChart (chartType, gameDay) =
        (chartType, ModelToDomainMapping.modelToGameDay gameDay) |> SixtySeconds.Services.ServiceActions.showChart
        
    let writeToSpreadsheet (sheetOptions, gameDay, team) =
        (sheetOptions, ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySeconds.Services.ServiceActions.writeToSpreadsheet >> mapError)