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

type SeasonTab =
    | SixtySeconds
    | Matrix

type State = {
    MaximumGames : int
    SixtySecondsResults : SeasonResultModel
    MatrixResults : MatrixSeasonModel
    Filter : RatingFilterModel
    ActiveTab : SeasonTab
    FilteredTable : TeamResultsTable
}

type Message =
    | GameCountChanged of int
    | FinalGameCountsChanged of bool
    | FinalDateChanged of DateTime option
    | FilterTable
    | TableUpdated of TeamResultsTable
    | TableNotUpdated of string
    | TabChanged of SeasonTab


let init secondsTable matrixTable =

    let gamesAmount = secondsTable |> SeasonResult.gamesAmount 
    {
        MaximumGames = gamesAmount

        SixtySecondsResults = secondsTable
        MatrixResults = matrixTable
        
        Filter =
            {
                GamesToCount = gamesAmount
                FinalDate = NotPlayedYet
                RatingOption = FinalGameCounts 
            }
        ActiveTab = SeasonTab.SixtySeconds
        FilteredTable = []
    },  GameCountChanged(gamesAmount) |> Cmd.ofMsg

let update message state =
    match message with
    | GameCountChanged games ->
        
        { state with Filter = {state.Filter with GamesToCount = games} }, FilterTable |> Cmd.ofMsg
        
    | FinalGameCountsChanged counts ->
        
        let maximumGames =
            if counts then state.SixtySecondsResults |> gamesAmount
            else state.SixtySecondsResults |> gamesAmount |> (+) -1
        
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
    
    | TabChanged newTab ->
        
        { state with ActiveTab = newTab; FilteredTable = []}, FilterTable |> Cmd.ofMsg
        
    | FilterTable ->
        
        let results =
            match state.ActiveTab with
            | SixtySeconds -> state.SixtySecondsResults
            | Matrix -> state.MatrixResults
        
        let ofSuccess res =
            match res with
            | Ok value -> TableUpdated value
            | Error str -> TableNotUpdated str
        
        state, Cmd.OfAsync.perform sixtySecondsApi.filterRating (state.Filter, results) ofSuccess
        
    | TableUpdated table -> {state with FilteredTable = table}, Cmd.none
    // TODO rewrite it
    | TableNotUpdated e -> failwith e

let render state dispatch =

    let lines =
        state.FilteredTable
        |> List.map (fun (team, points, place) -> tr[] [ td [] [str <| Place.toString place]; td [] [str team.Name]; td[] [str <| string points] ])

    let selectItems =
        [1..state.MaximumGames]
        |> List.map (fun i -> option [Value (string i)] [str <| string i])
        
    let selectFinalDate =
        state.SixtySecondsResults
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
                
            Tabs.tabs [Tabs.Size IsLarge]
                [
                    Tabs.tab
                        [
                            Tabs.Tab.IsActive (match state.ActiveTab with SixtySeconds -> true | _ -> false)
                            Tabs.Tab.Option.Props [OnClick (fun _ -> SixtySeconds |> TabChanged |> dispatch)]
                        ]
                        [
                            a [] [str "60 seconds"]
                        ]
                    Tabs.tab
                        [
                            Tabs.Tab.IsActive (match state.ActiveTab with Matrix -> true | _ -> false)
                            Tabs.Tab.Option.Props [OnClick (fun _ -> Matrix |> TabChanged |> dispatch)]
                        ]
                        [
                            a [] [str "Matrix"]
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