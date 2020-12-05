[<RequireQualifiedAccess>]
module GameDayPage

open Client
open Elmish

open Shared
open Shared.Models

open Fable.React
open Fulma

type ModalWindow =
    | Spreadsheet of WriteToSpreadsheetModalPage.SpreadsheetState
    | TeamPerformance of TeamPerformanceModalPage.TeamPerformanceState
    | Chart of ChartModalPage.ChartWindowState


type State =
    {
        GameDay : GameDayModel
        Rating : GameDayRating

        ModalWindow : ModalWindow option
    }

let init gameDay =
    {
        GameDay = gameDay
        Rating = Rating.ofGameDay gameDay

        ModalWindow = None
    }


type Message =

    | OpenWriteToSpreadsheet
    | WriteToSpreadsheetUpdated of WriteToSpreadsheetModalPage.SpreadsheetMessage
    | CloseWriteToSpreadsheet

    | OpenTeamPerformance
    | UpdateTeamPerformance of TeamPerformanceModalPage.Message
    | CloseTeamPerformance

    | OpenChartWindow
    | UpdateChartWindow of ChartModalPage.Message
    | CloseChartWindow

let update message model =
    match message, model.ModalWindow with
    | OpenWriteToSpreadsheet, None ->
        let innerState = WriteToSpreadsheetModalPage.init model.GameDay
        {model with ModalWindow = Some <| Spreadsheet(innerState) }, Cmd.none

    | WriteToSpreadsheetUpdated msg, Some (Spreadsheet spread) ->
        let newState, cmds = WriteToSpreadsheetModalPage.update msg spread
        {model with ModalWindow = Some <| Spreadsheet newState}, cmds |> Cmd.map WriteToSpreadsheetUpdated

    | CloseWriteToSpreadsheet, _ -> {model with ModalWindow = None; }, Cmd.none

    | OpenTeamPerformance, None ->
        let team = model.GameDay |> GameDay.teams |> Seq.head
        let performanceState, cmds = TeamPerformanceModalPage.init model.GameDay team
        {model with ModalWindow = Some <| TeamPerformance(performanceState) }, cmds |> Cmd.map UpdateTeamPerformance

    | UpdateTeamPerformance msg, Some (TeamPerformance pageState) ->
        let newState, cmds = TeamPerformanceModalPage.update msg pageState
        {model with ModalWindow = Some <| TeamPerformance(newState) }, cmds |> Cmd.map UpdateTeamPerformance

    | CloseTeamPerformance, _ -> {model with ModalWindow = None}, Cmd.none


    | OpenChartWindow, None ->
        let chartState, cmds = ChartModalPage.init(model.GameDay)
        {model with ModalWindow = Some <| Chart(chartState)}, cmds |> Cmd.map UpdateChartWindow
    | UpdateChartWindow msg, Some (Chart chartState) ->
        let newState, cmds = ChartModalPage.update msg chartState
        {model with ModalWindow = Some <| Chart(newState) }, cmds |> Cmd.map UpdateChartWindow
    | CloseChartWindow, _ -> {model with ModalWindow = None}, Cmd.none

    | _ -> model, Cmd.none

let render state dispatch =

    let writeToSpreadsheetActive, writeToSpreadsheetState =
        match state.ModalWindow with
        | Some (Spreadsheet s) -> true, s
        | _ -> false, WriteToSpreadsheetModalPage.init state.GameDay

    let teamPerformanceActive, teamPerformanceState =
        match state.ModalWindow with
        | Some (TeamPerformance tp) -> true, tp
        | _ -> false, TeamPerformanceModalPage.init state.GameDay (state.GameDay |> GameDay.teams |> Seq.head) |> fst

    let chartActive, chartState =
        match state.ModalWindow with
        | Some (Chart chart) -> true, chart
        | _ -> false, ChartModalPage.init state.GameDay |> fst

    let lines =
        state.Rating
        |> List.map (fun (team, points, place) -> tr [] [td [] [str <| Place.toString place]; td [] [str team.Name]; td[] [str <| string points]])
    div []
        [
            Level.level []
                [
                    Level.left []
                        [
                            Level.item []
                                [
                                    WriteToSpreadsheetModalPage.writeToSpreadsheetModal
                                        writeToSpreadsheetActive
                                        writeToSpreadsheetState
                                        (WriteToSpreadsheetUpdated >> dispatch)
                                        (fun _ -> CloseWriteToSpreadsheet |> dispatch)

                                    Button.button
                                        [Button.OnClick (fun _ -> OpenWriteToSpreadsheet |> dispatch)]
                                        [str "Write to spreadsheet"]
                                ]
                            Level.item []
                                [
                                    TeamPerformanceModalPage.render
                                        teamPerformanceActive
                                        teamPerformanceState
                                        (UpdateTeamPerformance >> dispatch)
                                        (fun _ -> CloseTeamPerformance |> dispatch)
                                    Button.button
                                        [Button.OnClick (fun _ -> OpenTeamPerformance |> dispatch)]
                                        [str "Team performance"]
                                ]
                            Level.item []
                                [
                                    ChartModalPage.render chartActive chartState (UpdateChartWindow >> dispatch) (fun _ -> CloseChartWindow |> dispatch)
                                    Button.button
                                        [Button.OnClick (fun _ -> OpenChartWindow |> dispatch)]
                                        [str "Show charts"]
                                ]
                        ]
                ]

            Table.table [Table.IsStriped; Table.IsHoverable;]
                [
                    thead []
                        [
                            tr []
                                [
                                    th [] [str "Position"]
                                    th [] [str "Name"]
                                    th [] [str "Points"]
                                ]
                        ]
                    tbody [] lines
                ]
        ]