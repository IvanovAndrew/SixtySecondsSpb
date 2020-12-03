[<RequireQualifiedAccess>]
module Client.TeamPerformanceModalPage

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Shared.Models

type TeamPerformanceState =
    {
        GameDay : GameDayModel
        TeamPerformance : TeamPerformance
    }

let calculateTeamPerformance gameDay team : TeamPerformance =
    {
        Team = team
        BestPlace = Team.bestPlace gameDay team
        WorstPlace = Team.worstPlace gameDay team
        BestStrike = Team.bestStrike gameDay team
        WorstStrike = Team.worstStrike gameDay team
        DifficultAnsweredQuestion = Team.difficultAnswered gameDay team |> fst |> (fun aq -> aq.Number)
        DifficultAnsweredQuestionCount = Team.difficultAnswered gameDay team |> snd
        SimplestWrongAnsweredQuestion = Team.simplestWrongAnswered gameDay team |> fst |> (fun aq -> aq.Number)
        SimplestWrongAnsweredQuestionCount = Team.simplestWrongAnswered gameDay team |> snd
    }

let init gameday team =
    {
        GameDay = gameday
        TeamPerformance = calculateTeamPerformance gameday team
    }, Cmd.none

type Message =
    | TeamChanged of int

let update message state =
    match message with
    | TeamChanged newTeamId ->
        let newTeam =
            state.GameDay
            |> GameDay.teams
            |> Seq.find (fun t -> t.Id = newTeamId)
        {state with TeamPerformance = calculateTeamPerformance state.GameDay newTeam}, Cmd.none



let render isActive state dispatch closeDisplay =

    let selectItems =
        state.GameDay
        |> GameDay.teams
        |> Seq.map (fun team -> option [Value (string team.Id) ] [str team.Name])

    let text =
            let perf = state.TeamPerformance
            [
                sprintf "Best place %s (after %d question)" <| Place.toString perf.BestPlace.Place <| perf.BestPlace.Question
                sprintf "Worst place %s (after %d question)" <| Place.toString perf.WorstPlace.Place <| perf.WorstPlace.Question
                sprintf "Best strike %d" <| (perf.BestStrike.Count |> Option.defaultValue 0)
                sprintf "Worst strike %d" <| (perf.WorstStrike.Count |> Option.defaultValue 0)
                sprintf "Difficult answered question %d (%d right answers)" perf.DifficultAnsweredQuestion perf.DifficultAnsweredQuestionCount
                sprintf "Simplest unanswered question %d (%d right answers)" perf.SimplestWrongAnsweredQuestion perf.SimplestWrongAnsweredQuestionCount
            ]
            |> List.map str
            |> List.map (fun e -> p [] [e])

    Modal.modal
        [Modal.IsActive isActive]
        [
            Modal.background [Props [OnClick closeDisplay]] []
            Modal.Card.card []
                [
                    Modal.Card.head []
                        [
                            Modal.Card.title [] [str "Team performance"]
                            Delete.delete [Delete.OnClick closeDisplay] []
                        ]
                    Modal.Card.body []
                        [
                            Field.div []
                                [
                                    Label.label [] [ str "Team" ]
                                    Select.select []
                                        [
                                            select [OnClick (fun x -> x.Value |> int |> TeamChanged |> dispatch)]
                                                selectItems
                                        ]
                                ]
                            Content.content [] text
                        ]
                ]
        ]