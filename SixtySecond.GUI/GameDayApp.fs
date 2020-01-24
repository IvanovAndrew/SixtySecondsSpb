module GameDayApp

open Domain
open Utils
open SpreadsheetApp
open Elmish.WPF
open System.Windows

type Model = 
    {
        GameDay : GameDay
        TeamId : string
        SpreadsheetWindow : SpreadsheetApp.Model option
    }

let updateTeamId teamId window = {window with TeamId = teamId}

let validateTeamId (gameDay : GameDay) teamId = 
        
    let findTeam teamId = 
        let teamOption = 
            gameDay
            |> GameDay.teams
            |> Seq.tryFind(fun t -> t.ID = teamId)
    
        match teamOption with 
        | Some team -> Result.Ok team
        | None -> Error "Team not found"

    teamId 
    |> PositiveNum.ofString
    |> Result.bind findTeam

        
    
let showCharts team gameDay = 
    Program.showPointsQuestionByQuestion gameDay [team]
    Program.showPlacesQuestionByQuestion gameDay [team]

type Message = 
    | TeamIdEntered of teamId : string
    | ShowCharts of team : Team
    | OpenSpreadsheetWindow of Team
    | GoogleSpreadsheetCloseRequested
    | SpreadsheetMessage of SpreadsheetApp.Message
    

let update message model = 
    match message with
    | TeamIdEntered newTeamId -> updateTeamId newTeamId model
    | ShowCharts team -> 
    
        model.GameDay
        |> showCharts team

        model

    | OpenSpreadsheetWindow team -> 
        
        let subModel = 
            team
            |> Program.teamGameDay model.GameDay
            |> SpreadsheetApp.init 
            
        {model with SpreadsheetWindow = Some subModel}
    | SpreadsheetMessage message' -> 
        { 
            model with 
                SpreadsheetWindow = 
                    model.SpreadsheetWindow 
                    |> Option.map (SpreadsheetApp.update message')
        }
    | GoogleSpreadsheetCloseRequested -> {model with SpreadsheetWindow = None}
    

let bindings (wrap : Message -> 'a) = 
    (fun () -> [
        "TeamId" |> Binding.twoWayValidate(
                (fun m -> m.TeamId),
                TeamIdEntered >> wrap,
                (fun m -> validateTeamId m.GameDay m.TeamId)
            )
        
        "ShowCharts" |> Binding.cmdIf(
            fun model -> 
                model.TeamId 
                |> validateTeamId model.GameDay 
                |> Result.map (ShowCharts >> wrap))
        
        "OpenSpreadsheetWindow" |> Binding.cmdIf(
            fun model -> 
                model.TeamId 
                |> validateTeamId model.GameDay 
                |> Result.map(OpenSpreadsheetWindow >> wrap))

        "SpreadsheetWindow" |> Binding.subModelWin(
            (fun m -> m.SpreadsheetWindow |> WindowState.ofOption), 
            snd, 
            id,
            SpreadsheetApp.bindings (SpreadsheetMessage >> wrap),
            (fun () -> SixtySeconds.Views.GoogleSpreadsheetWindow(Owner = Application.Current.MainWindow)),
            onCloseRequested = wrap(GoogleSpreadsheetCloseRequested),
            isModal = true)
    ])