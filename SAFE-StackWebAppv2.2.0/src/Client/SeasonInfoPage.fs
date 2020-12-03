[<RequireQualifiedAccess>]
module SeasonInfoPage

open Shared
open Shared.Models

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

type State = {
    GamesToCount : int
    MaximumGames : int
    SeasonTable : SeasonTableModel
    FilteredSeasonTable : TeamRatingPosition<decimal> seq
}

type Message =
    | FilterTable of int

let init (seasonTable : SeasonTableModel) =
    {
        GamesToCount = seasonTable.GamesCount
        MaximumGames = seasonTable.GamesCount
        SeasonTable = seasonTable
        FilteredSeasonTable = Rating.topNResult seasonTable.GamesCount seasonTable
    }

let update message state =
    match message with
    | FilterTable games ->
        let table = state.SeasonTable |> Rating.topNResult games
        { state with GamesToCount = games; FilteredSeasonTable = table }, Cmd.none


let render state dispatch =

    let lines =
        state.FilteredSeasonTable
        |> Seq.map (fun (team, points, place) -> tr[] [ td [] [str <| Place.toString place]; td [] [str team.Name]; td[] [str <| string points] ])

    let dropDownItems =
        [1..state.MaximumGames]
        |> List.map (fun i -> Dropdown.Item.a [Dropdown.Item.Option.Props [OnClick (fun x -> i |> FilterTable |> dispatch)]] [ str <| string i ])

    let selectItems =
        [1..state.MaximumGames]
        |> List.map (fun i -> option [Value (string i)] [str <| string i])


    div []
        [
            Level.level []
                [
                    Level.left []
                        [
                            Level.item [] [str "Games to count"]
                            Level.item []
                                [
                                    Select.select []
                                            [
                                                select
                                                    [
                                                        OnClick (fun x -> x.Value |> int |> FilterTable |> dispatch)
                                                        DefaultValue state.MaximumGames
                                                    ]
                                                    selectItems
                                            ]
                                ]
                        ]
                ]

            Table.table [Table.IsBordered; Table.IsStriped; Table.IsHoverable; ]
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