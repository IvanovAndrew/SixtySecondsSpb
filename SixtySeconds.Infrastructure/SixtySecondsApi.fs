namespace SixtySeconds.Infrastructure

module SixtySecondsApi =
    
    open SixtySeconds
    open Shared
    
    let gameDayToDto gdResult =
        async {
            let! result = gdResult  
            
            return result |> Result.map Models.gameDayToModel |> Result.mapError SixtySeconds.Common.ErrorMessages.errorToString 
        }
        
    let totalToDto totalResult =
        async {
            let! result = totalResult
            
            return result |> Result.map Models.seasonTableToModel |> Result.mapError SixtySeconds.Common.ErrorMessages.errorToString
        }
        
    let mapError result =
        async {
            let! res = result
            return res |> Result.mapError SixtySeconds.Common.ErrorMessages.errorToString
        }
    
    let gameDayRating arg =
           arg |> (SixtySecondsWorkflow.gameDayRating >> SixtySecondsProgramInterpreter.interpretSimple)
                   

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
        arg |> (SixtySecondsWorkflow.parseTotal >> SixtySecondsProgramInterpreter.interpret >> totalToDto)
        
    let parseGameDay arg =
        arg |> (SixtySecondsWorkflow.parseGameDay >> SixtySecondsProgramInterpreter.interpret >> gameDayToDto)
        
    let showChart (chartType, gameDay) =
        (chartType, ModelToDomainMapping.modelToGameDay gameDay) |> SixtySeconds.Services.ServiceActions.showChart
        
    let writeToSpreadsheet (sheetOptions, gameDay, team) =
        (sheetOptions, ModelToDomainMapping.modelToGameDay gameDay, ModelToDomainMapping.modelToTeam team)
        |> (SixtySeconds.Services.ServiceActions.writeToSpreadsheet >> mapError)