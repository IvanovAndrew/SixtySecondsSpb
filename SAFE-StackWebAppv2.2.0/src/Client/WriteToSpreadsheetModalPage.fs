[<RequireQualifiedAccess>]
module Client.WriteToSpreadsheetModalPage

open ServerApi

open Shared
open Shared.Models

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

let tryParseInt (s : string) =
    s
    |> (fun s -> if s <> "" then s |> int |> Some else None)

let defaultSheetOptions =
    {
        FirstQuestion = 101//Config.load FirstQuestion |> int
        TeamAnswered = "F"//Config.load TeamAnswered
        Answered = "G"//Config.load RightAnswers
        Place = "H"//Config.load Place
        Distance = "I" //Config.load Distance
    }

type SpreadsheetState =
    {
        Options : SpreadsheetOptions
        GameDay : GameDayModel
        Team : TeamModel option
        Status : Result<string, string>
    }

let init gameDay : SpreadsheetState =
    {
        GameDay  = gameDay
        Options =
            {
                Id = "1WDsY7US9TunbCc2PZnDMvGBSsgePBp5eXITX7LHeT-8"//Config.load SpreadsheetUrl
                SheetName = gameDay.Name
                ColumnOptions = defaultSheetOptions
            }
        Team = None
        Status = Ok ""
    }


type SpreadsheetMessage =
    | NewId of newValue : string
    | SheetNameChanged of newValue : string
    | TeamAnsweredColumnChanged of newValue : string
    | RightAnswersColumnChanged of newValue : string
    | PlacesColumnChanged of newValue : string
    | DistanceColumnChanged of newValue : string
    | FirstQuestionRowChanged of newValue : int
    | TeamSelected of int
    | WriteToSpreadsheetRequested
    | DataWritten
    | DataNotWritten of string

let update change state =

    match change with
    | NewId value -> {state with Options = {state.Options with Id = value}}, Cmd.none
    | SheetNameChanged value -> {state with Options = {state.Options with SheetName = value}}, Cmd.none
    | TeamAnsweredColumnChanged value ->
            {
                state with Options = { state.Options with ColumnOptions = {state.Options.ColumnOptions with TeamAnswered = value}}
            }, Cmd.none
    | RightAnswersColumnChanged value ->
            {
                state with Options = { state.Options with ColumnOptions = {state.Options.ColumnOptions with Answered = value}}
            }, Cmd.none
    | PlacesColumnChanged value ->
            {
                state with Options = {state.Options with ColumnOptions = {state.Options.ColumnOptions with Place = value}}
            }, Cmd.none
    | DistanceColumnChanged value ->
            {
                state with Options = { state.Options with ColumnOptions = {state.Options.ColumnOptions with Distance = value}}
            }, Cmd.none
    | FirstQuestionRowChanged value ->
            {
                state with Options = { state.Options with ColumnOptions = {state.Options.ColumnOptions with FirstQuestion = value}}
            }, Cmd.none
    | TeamSelected newTeamId ->
        let team =
            state.GameDay
            |> GameDay.teams
            |> Seq.find (fun t -> t.Id = newTeamId)
        {state with Team = Some team}, Cmd.none

    | WriteToSpreadsheetRequested ->

        let ofSuccess = function
            | Ok _ -> DataWritten
            | Error e -> DataNotWritten e

        let ofError exn = DataNotWritten <| exn.ToString()
        let args = state.Options, state.GameDay, state.Team.Value

        {state with Status = Ok "Waiting"}, Cmd.OfAsync.either sixtySecondsApi.writeToSpreadsheet args ofSuccess ofError

    | DataWritten _ ->

        {state with Status = Ok "Data written"}, Cmd.none

    | DataNotWritten e -> {state with Status = Error e}, Cmd.none


// Render the modal
let writeToSpreadsheetModal isActive state dispatch closeDisplay =

    let selectItems =
        state.GameDay
        |> GameDay.teams
        |> Seq.map (fun team -> option [Value (string team.Id) ] [str team.Name])

    let firstQuestionRowChangeHandler (x : Browser.Types.Event) =
        let number =
            match tryParseInt x.Value with
            | Some int -> int
            | None -> 0
        number |> FirstQuestionRowChanged |> dispatch

    Modal.modal [Modal.IsActive isActive]
        [
            Modal.background [Props [OnClick closeDisplay]] []
            Modal.Card.card []
                [
                    Modal.Card.head []
                        [
                            Modal.Card.title [] [str "Write to spreadsheet"]
                            Delete.delete [Delete.OnClick closeDisplay] []
                        ]
                    Modal.Card.body []
                        [
                            form []
                                 [
                                    Field.div []
                                        [
                                            Label.label [] [ str "Team" ]
                                            Select.select []
                                                [
                                                    select [OnClick (fun x -> x.Value |> int |> TeamSelected |> dispatch)]
                                                        selectItems
                                                ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Spreadsheet id"]
                                            Control.div []
                                                [
                                                    Input.text [
                                                        Input.ValueOrDefault state.Options.Id
                                                        Input.OnChange (fun e -> e.Value |> NewId |> dispatch)
                                                    ]
                                                ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Sheet name"]
                                            Control.div []
                                                [
                                                    Input.text
                                                        [
                                                            Input.ValueOrDefault state.Options.SheetName
                                                            Input.OnChange (fun e -> e.Value |> SheetNameChanged |> dispatch)
                                                        ]
                                                ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "First question row"]
                                            Control.div [] [
                                                Input.text
                                                    [
                                                        Input.ValueOrDefault <| string state.Options.ColumnOptions.FirstQuestion
                                                        Input.OnChange firstQuestionRowChangeHandler
                                                    ]
                                            ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Answered?"]
                                            Control.div [] [
                                                Input.text
                                                    [
                                                        Input.ValueOrDefault state.Options.ColumnOptions.Answered
                                                        Input.OnChange (fun e -> e.Value |> TeamAnsweredColumnChanged |> dispatch)
                                                    ]
                                            ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Right answers"]
                                            Control.div [] [
                                                Input.text
                                                    [
                                                        Input.ValueOrDefault state.Options.ColumnOptions.TeamAnswered
                                                        Input.OnChange (fun e -> e.Value |> RightAnswersColumnChanged |> dispatch)
                                                    ]
                                            ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Place"]
                                            Control.div [] [
                                                Input.text
                                                    [
                                                        Input.ValueOrDefault state.Options.ColumnOptions.Place
                                                        Input.OnChange (fun e -> e.Value |> PlacesColumnChanged |> dispatch)
                                                    ]
                                            ]
                                        ]
                                    Field.div []
                                        [
                                            Label.label [] [str "Distance"]
                                            Control.div [] [
                                                Input.text
                                                    [
                                                        Input.ValueOrDefault state.Options.ColumnOptions.Distance
                                                        Input.OnChange (fun e -> e.Value |> DistanceColumnChanged |> dispatch)
                                                    ]
                                            ]
                                        ]
                                ]
                        ]

                    Modal.Card.foot []
                        [
                            Button.button [Button.Color IsSuccess; Button.OnClick (fun _ -> WriteToSpreadsheetRequested |> dispatch)] [str "Save changes"]
                            Button.button [Button.OnClick closeDisplay] [str "Cancel"]
                            Help.help
                                    [Help.Color (match state.Status with Ok _ -> IsSuccess | _ -> IsDanger)]
                                    [str (match state.Status with Ok i -> i | Error e -> e)]
                        ]
                ]
        ]