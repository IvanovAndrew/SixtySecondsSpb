[<RequireQualifiedAccess>]
module Client.ChartModalPage

open Client
open ServerApi
open Elmish

open Shared
open Shared.Models

open Fable.React
open Fable.React.Props
open Fulma
open Fulma.Extensions.Wikiki

type ChartWindowState =
    {
        GameDay : GameDayModel
        SelectedTeam : TeamModel option
        ChartType : ChartType
        Status : Result<string, string>
    }

let init gameDay =
    {
       GameDay = gameDay
       SelectedTeam = None
       ChartType = ChartType.Answers <| BestTeamsOnly 3
       Status = Ok ""
    }, Cmd.none

type Message =
    | ChartTypeChanged of ChartType
    | ShowChart
    | TeamSelected of int
    | FailStatus of string

let update message model =

    match message with
    | ChartTypeChanged chartType -> {model with ChartType = chartType}, Cmd.none
    | ShowChart ->

        let ofError exn = FailStatus <| exn.ToString()
        let args = model.ChartType, model.GameDay

        {model with Status = Ok "Waiting"}, Cmd.OfAsync.attempt sixtySecondsApi.showChart args ofError
    | TeamSelected newTeamId ->
        let team =
            model.GameDay
            |> GameDay.teams
            |> Seq.find (fun t -> t.Id = newTeamId)
        {model with SelectedTeam = Some team}, Cmd.none
    | FailStatus e -> {model with Status = Error e}, Cmd.none


let render isActive state dispatch closeDisplay =

    let selectItems =
        state.GameDay
        |> GameDay.teams
        |> Seq.map (fun team -> option [Value (string team.Id) ] [str team.Name])

    Modal.modal
        [Modal.IsActive isActive]
        [
            Modal.background [Props [OnClick closeDisplay]] []
            Modal.Card.card []
                [
                    Modal.Card.head []
                        [
                            Modal.Card.title [] [ str "Show charts" ]
                            Delete.delete [ Delete.OnClick closeDisplay ] []
                        ]
                    Modal.Card.body []
                        [
                            div []
                                [
                                    Label.label [] [str "Team"]
                                    select [OnClick (fun x -> x.Value |> int |> TeamSelected |> dispatch)]
                                        selectItems
                                    Column.column []
                                        [
                                            b [] [str "Chart type"]
                                            Checkradio.radio
                                                [
                                                    Checkradio.Name "inline"
                                                    Checkradio.Id "chartTypeRadio-places"
                                                    Checkradio.Checked true
                                                    Checkradio.OnChange (fun _ -> state.SelectedTeam |> Option.map (fun t -> [t.Id]) |> Option.defaultValue [-1] |> CustomTeamsOnly |> Places |> ChartTypeChanged |> dispatch)
                                                ] [str "Places"]
                                            Checkradio.radio
                                                [
                                                    Checkradio.Name "inline"
                                                    Checkradio.Id "chartTypeRadio-answers"
                                                    Checkradio.OnChange (fun _ -> state.SelectedTeam |> Option.map (fun t -> [t.Id]) |> Option.defaultValue [-1] |> CustomTeamsOnly |> Answers |> ChartTypeChanged |> dispatch)
                                                ] [str "Answers"]
                                        ]
                                ]

                        ]
                    Modal.Card.foot []
                        [
                            Button.button [ Button.Color IsSuccess; Button.OnClick (fun _ -> ShowChart |> dispatch) ] [str "Show chart"]
                            Button.button [ Button.OnClick closeDisplay] [str "Cancel"]
                            Help.help
                                    [ Help.Color (match state.Status with Ok _ -> IsSuccess | _ -> IsDanger) ]
                                    [ str (match state.Status with Ok i -> i | Error e -> e) ]
                        ]
                ]
        ]