[<RequireQualifiedAccess>]
module SeasonInfoPage

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Client.ServerApi

open Shared
open Shared.Api
open Shared.Models
open Shared.SeasonResult

open System

type State = {
    MaximumGames : int
    SeasonResults : SeasonResultModel
    Filter : RatingFilterModel
    FilteredSeasonTable : (TeamModel * decimal * PlaceModel) list
}

type Message =
    | GameCountChanged of int
    | FinalGameCountsChanged of bool
    | FinalDateChanged of DateTime option
    | FilterTable
    | TableUpdated of (TeamModel * decimal * PlaceModel) list
    | TableNotUpdated of string

let init (seasonTable : SeasonResultModel) =

    let gamesAmount = seasonTable |> SeasonResult.gamesAmount 
    {
        MaximumGames = gamesAmount
        SeasonResults = seasonTable
        Filter =
            {
                GamesToCount = gamesAmount
                FinalDate = NotPlayedYet
                RatingOption = FinalGameCounts 
            }
        FilteredSeasonTable = [] 
    },  GameCountChanged(gamesAmount) |> Cmd.ofMsg

let update message state =
    match message with
    | GameCountChanged games ->
        
        { state with Filter = {state.Filter with GamesToCount = games} }, FilterTable |> Cmd.ofMsg
        
    | FinalGameCountsChanged counts ->
        
        let maximumGames =
            if counts then state.SeasonResults |> gamesAmount
            else state.SeasonResults |> gamesAmount |> (+) -1
        
        let gamesToCount =
            if counts then state.Filter.GamesToCount
            else
                if state.Filter.GamesToCount > maximumGames then maximumGames else state.Filter.GamesToCount
        let newFilter =
            {
                state.Filter with
                    GamesToCount = gamesToCount;
                    RatingOption = if counts then FinalGameCounts else FinalGameDoesntCount
            } 
                
        { state with Filter = newFilter; MaximumGames = maximumGames }, FilterTable |> Cmd.ofMsg
        
    | FinalDateChanged dateOption ->
        
        let finalDate =
            match dateOption with
            | Some date -> PlayedAlready date
            | None -> NotPlayedYet
        {state with Filter = {state.Filter with FinalDate = finalDate}}, FilterTable |> Cmd.ofMsg
        
    | FilterTable ->
        let ofSuccess res =
            match res with
            | Ok value -> TableUpdated value
            | Error str -> TableNotUpdated str
        let ofError exn = TableNotUpdated <| string exn
        state, Cmd.OfAsync.either sixtySecondsApi.filterRating (state.Filter, state.SeasonResults) ofSuccess ofError
        
    | TableUpdated table -> {state with FilteredSeasonTable = table}, Cmd.none
    // TODO rewrite it
    | TableNotUpdated e -> failwith e

let render state dispatch =

    let lines =
        state.FilteredSeasonTable
        |> Seq.map (fun (team, points, place) -> tr[] [ td [] [str <| Place.toString place]; td [] [str team.Name]; td[] [str <| string points] ])

    let selectItems =
        [1..state.MaximumGames]
        |> List.map (fun i -> option [Value (string i)] [str <| string i])
        
    let selectFinalDate =
        state.SeasonResults
        |> gameDates
        |> List.map (fun date -> option [Value (Some date)] [str <| sprintf "%d.%d.%d" date.Day date.Month date.Year])
        |> List.append [option [Value(None)] [str <| "Not played yet"]]
            
    let finalGamesCount = match state.Filter.RatingOption with FinalGameDoesntCount _ -> false | _ -> true

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
                                                        OnClick (fun x -> x.Value |> int |> GameCountChanged |> dispatch)
                                                        Value state.Filter.GamesToCount
                                                    ]
                                                    selectItems
                                            ]
                                ]
                                
                            Level.item [] [str "Final game"]
                            Level.item []
                                [
                                    Select.select []
                                            [       
                                                select
                                                    [
                                                        OnClick (fun x -> x.Value |> Utils.tryParseDateTime |> FinalDateChanged |> dispatch)
                                                        Value (match state.Filter.FinalDate with PlayedAlready d -> Some d | NotPlayedYet -> None)
                                                    ]
                                                    selectFinalDate
                                            ]
                                ]
                        ]
                ]
            Level.level []
                [
                    Level.left []
                        [
                            Level.item []
                                [
                                    Checkbox.checkbox []
                                        [
                                            Checkbox.input
                                                [
                                                    GenericOption.Props
                                                        [
                                                            Checked finalGamesCount
                                                            OnClick (fun x -> x.Checked |> FinalGameCountsChanged |> dispatch)
                                                        ]
                                                ]
                                            str "Final game counts towards to the season"
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