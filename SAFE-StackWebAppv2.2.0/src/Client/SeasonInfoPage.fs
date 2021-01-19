[<RequireQualifiedAccess>]
module SeasonInfoPage

open Elmish
open Fable.React
open Fable.React.Props
open Fulma

open Client.ServerApi

open Shared
open Shared.Models
open Shared.SeasonResult

open System

type MatrixSubModel = {
    MaximumGames : int
    Results : Map<TeamModel, GamedayPointsModel list>
    Filter: MatrixFilter
    Table : TeamResultsTable
}

type SixtySecondsSubModel =
    {
        MaximumGames : int
        Results : Map<TeamModel, GamedayPointsModel list>
        Filter : SixtySecondsFilter
        Table  : TeamResultsTable
    }

type ActiveSubmodel =
    | SixtySeconds of SixtySecondsSubModel
    | Matrix of MatrixSubModel

type Model = {
    MatrixSubModel : MatrixSubModel
    SixtySecondsSubModel : SixtySecondsSubModel
    ActiveSubmodel : ActiveSubmodel
}

type Message =
    | GameCountChanged of int
    | FinalGameCountsChanged of bool
    | FinalDateChanged of DateTime option
    | FilterSixtySecondsTable
    | FilterMatrixTable
    | SixtySecondsTableUpdated of TeamResultsTable
    | MatrixTableUpdated of TeamResultsTable
    | TableUpdated of TeamResultsTable
    | TableNotUpdated of string
    | TabChanged

let init secondsTable matrixTable =

    let matrixFilter : MatrixFilter = { GamesToCount = matrixTable |> gamesAmount } 
    
    let matrixSubModel : MatrixSubModel =
        {
            MaximumGames = matrixTable |> gamesAmount
            Results = matrixTable
            Filter = matrixFilter
            Table = []
        }
    
    let sixtySecondsSubModel =
        {
            MaximumGames = secondsTable |> gamesAmount
            Results = secondsTable
            Filter = { GamesToCount = secondsTable |> gamesAmount; FinalDate = NotPlayedYet; RatingOption = FinalGameCounts }
            Table = []
        }
    
    {
        MatrixSubModel = matrixSubModel
        SixtySecondsSubModel = sixtySecondsSubModel

        ActiveSubmodel = SixtySeconds sixtySecondsSubModel
    }, FilterSixtySecondsTable |> Cmd.ofMsg
    
    

let update message model =
    match message, model.ActiveSubmodel with
    | GameCountChanged games, SixtySeconds submodel ->
        let updatedSubModel = { submodel with Filter = {submodel.Filter with GamesToCount = games}} 
        { model with ActiveSubmodel = SixtySeconds updatedSubModel }, FilterSixtySecondsTable |> Cmd.ofMsg
        
    | GameCountChanged games, Matrix submodel ->
        let updatedSubModel = { submodel with Filter = {submodel.Filter with GamesToCount = games}} 
        { model with ActiveSubmodel = Matrix updatedSubModel }, FilterMatrixTable |> Cmd.ofMsg
        
    | FinalGameCountsChanged counts, SixtySeconds submodel ->
        
        let updatedSubmodel = 
            let maximumGames =
                if counts then submodel.Results |> gamesAmount
                else submodel.Results |> gamesAmount |> (+) -1
            
            let gamesToCount =
                if counts then submodel.Filter.GamesToCount
                else
                    if submodel.Filter.GamesToCount > maximumGames then maximumGames else submodel.Filter.GamesToCount
            let newFilter =
                {
                    submodel.Filter with
                        GamesToCount = gamesToCount;
                        RatingOption = if counts then FinalGameCounts else FinalGameDoesntCount
                }
                
            { submodel with Filter = newFilter; MaximumGames = maximumGames}
                
        { model with ActiveSubmodel = SixtySeconds(updatedSubmodel) }, FilterSixtySecondsTable |> Cmd.ofMsg
    
    | FinalGameCountsChanged counts, Matrix submodel ->
        
        let updatedSubmodel = 
            let maximumGames =
                if counts then submodel.Results |> gamesAmount
                else submodel.Results |> gamesAmount |> (+) -1
            
            let gamesToCount =
                if counts then submodel.Filter.GamesToCount
                else
                    if submodel.Filter.GamesToCount > maximumGames then maximumGames else submodel.Filter.GamesToCount
            let newFilter = { submodel.Filter with GamesToCount = gamesToCount; }
                
            { submodel with Filter = newFilter; MaximumGames = maximumGames}
                
        { model with ActiveSubmodel = Matrix updatedSubmodel }, FilterMatrixTable |> Cmd.ofMsg
        
    | FinalDateChanged dateOption, SixtySeconds submodel ->
        
        let updatedSubmodel = 
            let finalDate =
                match dateOption with
                | Some date -> PlayedAlready date
                | None -> NotPlayedYet
            {submodel with Filter = {submodel.Filter with FinalDate = finalDate}}
            
        {model with ActiveSubmodel = SixtySeconds updatedSubmodel }, FilterSixtySecondsTable |> Cmd.ofMsg
    
    | TabChanged, SixtySeconds submodel ->
        
        let nextCmd = 
            match model.MatrixSubModel.Table with
            | [] -> FilterMatrixTable
            | _ -> FilterSixtySecondsTable 
        { model with SixtySecondsSubModel = submodel; ActiveSubmodel = Matrix model.MatrixSubModel;}, nextCmd |> Cmd.ofMsg 
        
    | TabChanged, Matrix submodel ->
        
        { model with MatrixSubModel = submodel; ActiveSubmodel = SixtySeconds model.SixtySecondsSubModel; }, FilterMatrixTable |> Cmd.ofMsg
        
    | FilterSixtySecondsTable, SixtySeconds submodel ->
        
        let results = model.SixtySecondsSubModel.Results
        
        let ofSuccess res =
            match res with
            | Ok value -> TableUpdated value
            | Error str -> TableNotUpdated str
        
        model, Cmd.OfAsync.perform sixtySecondsApi.filterRating (SixtySecondsFilter submodel.Filter, results) ofSuccess
        
    | FilterMatrixTable, Matrix submodel ->
        
        let results = model.MatrixSubModel.Results
        
        let ofSuccess res =
            match res with
            | Ok value -> TableUpdated value
            | Error str -> TableNotUpdated str
        
        model, Cmd.OfAsync.perform sixtySecondsApi.filterRating (MatrixFilter submodel.Filter, results) ofSuccess
        
    | SixtySecondsTableUpdated table, _ -> {model with SixtySecondsSubModel = {model.SixtySecondsSubModel with Table = table}}, Cmd.none
    | MatrixTableUpdated table, _ -> {model with MatrixSubModel = ({model.MatrixSubModel with Table = table})}, Cmd.none
    | TableUpdated table, SixtySeconds ss -> {model with ActiveSubmodel = SixtySeconds ({ss with Table = table})}, Cmd.none
    | TableUpdated table, Matrix m -> {model with ActiveSubmodel = Matrix ({m with Table = table})}, Cmd.none
    // TODO rewrite it
    | TableNotUpdated e, _ -> failwith e
    | _ -> model, Cmd.none

let render model dispatch =

    let lines =
        let table = 
            match model.ActiveSubmodel with
            | SixtySeconds submodel -> submodel.Table
            | Matrix submodel -> submodel.Table
        table
        |> List.map (fun (team, points, place) -> tr[] [ td [] [str <| Place.toString place]; td [] [str team.Name]; td[] [str <| string points] ])

    let selectItems =
        let maximumGames =
            match model.ActiveSubmodel with
            | SixtySeconds submodel -> submodel.MaximumGames
            | Matrix submodel -> submodel.MaximumGames
        [1..maximumGames]
        |> List.map (fun i -> option [Value (string i)] [str <| string i])
        
    let selectFinalDate =
        let results =
            match model.ActiveSubmodel with
            | SixtySeconds ss -> ss.Results
            | Matrix m -> m.Results
        results
        |> gameDates
        |> List.map (fun date -> option [Value (Some date)] [str <| sprintf "%d.%d.%d" date.Day date.Month date.Year])
        |> List.append [option [Value(None)] [str <| "Not played yet"]]
            
    let finalGamesCount =
        match model.ActiveSubmodel with
        | SixtySeconds ss ->
            match ss.Filter.RatingOption with
            | FinalGameDoesntCount _ -> false
            | _ -> true
        | Matrix _ -> false
        
    let renderGamesToCount submodel =
        
        let gamesToCount =
            match submodel with
            | SixtySeconds ss -> ss.Filter.GamesToCount
            | Matrix m -> m.Filter.GamesToCount
        
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
                                                        Value gamesToCount 
                                                    ]
                                                    selectItems
                                            ]
                                ]
                        ]
                ]

    let renderFinalGameDate submodel =
        match submodel with
        | Matrix _ -> div [][]
        | SixtySeconds ss -> 
        
            Level.level []
                    [
                        Level.left []
                            [
                                Level.item [] [str "Final game"]
                                Level.item []
                                    [
                                        Select.select []
                                                [       
                                                    select
                                                        [
                                                            OnClick (fun x -> x.Value |> Utils.tryParseDateTime |> FinalDateChanged |> dispatch)
                                                            DefaultValue (match ss.Filter.FinalDate with PlayedAlready d -> Some d | NotPlayedYet -> None)
                                                        ]
                                                        selectFinalDate
                                                ]
                                    ]
                            ]
                    ]
    
    let renderFinalGameCounts submodel =
        match submodel with
        | Matrix _ -> div [] []
        | SixtySeconds _ -> 
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
    
    
    div []
        [
            Tabs.tabs []
                [
                    Tabs.tab
                        [
                            Tabs.Tab.IsActive (match model.ActiveSubmodel with SixtySeconds _ -> true | _ -> false)
                            Tabs.Tab.Option.Props [OnClick (fun _ -> TabChanged |> dispatch)]
                        ]
                        [
                            a [] [str "60 seconds"]
                        ]
                    Tabs.tab
                        [
                            Tabs.Tab.IsActive (match model.ActiveSubmodel with Matrix _ -> true | _ -> false)
                            Tabs.Tab.Option.Props [OnClick (fun _ -> TabChanged |> dispatch)]
                        ]
                        [
                            a [] [str "Matrix"]
                        ]
                ]
                
            renderGamesToCount model.ActiveSubmodel
            renderFinalGameDate model.ActiveSubmodel
            renderFinalGameCounts model.ActiveSubmodel
                
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
