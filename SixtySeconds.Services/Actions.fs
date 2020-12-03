namespace SixtySeconds.Services

open SixtySeconds
open SixtySeconds.Common.CommonTypes

module ServiceActions = 

    open SixtySeconds.Services
    open Domain
    open Actions
    open Shared.Models
    
    let showChart (chartType, gameDay : GameDay) =
        let teamsToShow input =
            
            let findCustomTeams idList =
                idList
                |> Seq.map PositiveNum.ofConst
                |> Seq.map (fun x -> gameDay |> GameDay.teams |> Seq.find (fun team -> team.ID = x))
        
            match input with
            | CustomTeamsOnly customTeams -> findCustomTeams customTeams
            | BestTeamsOnly bestTeams -> gameDay |> Rating.ofGameDay |> Rating.leadingTeams (PositiveNum.ofConst bestTeams) 
            | CustomTeamsAndBestTeams (customTeams, bestTeams) ->  
                
                gameDay
                |> Rating.ofGameDay
                |> Rating.leadingTeams (PositiveNum.ofConst bestTeams)
                |> Seq.append (findCustomTeams customTeams)
                |> Seq.distinct
            
        match chartType with
        | Answers options ->
            options
            |> teamsToShow
            |> Chart.showPointsQuestionByQuestion gameDay
            
        | Places options ->
            options
            |> teamsToShow
            |> Chart.showPlacesQuestionByQuestion gameDay
    
    let writeToSpreadsheet (sheetOptions, gameDay, team) =
        team
        |> DataToWrite.fromGameDay gameDay
        |> SpreadsheetWriter.write sheetOptions