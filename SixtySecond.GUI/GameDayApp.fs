module GameDayApp

open Domain
open Utils
open Elmish.WPF
open Utils.PositiveNum
open SpreadsheetWriter

type Model = 
    {
        GameDay : GameDay
        // Think about name
        TeamIds : string
        BestTeams : string
        ChartsErrorStatus : string option
        // Think about name
        TeamId : string
        SpreadSheetId : string
        SheetName : string
        SheetOptions : SheetOptions
        Status : Result<string, string> option
    }
    
type ShowChartsInput =
    | CustomTeamsOnly of Team list
    | BestTeamsOnly of PositiveNum
    | CustomTeamsAndBestTeams of teams : Team list * bestTeams : PositiveNum
    
let defaultSheetOptions = 
        {
            FirstQuestion = 3
            TeamAnswered = "E"
            Answered = "F"
            Place = "G"
            Distance = "H"
        }
        
let init gameDay =
    {
        GameDay = gameDay
        TeamIds = ""
        BestTeams = ""
        ChartsErrorStatus = None
        SpreadSheetId = "" 
        SheetName = ""
        TeamId = ""
        SheetOptions = defaultSheetOptions
        Status = None
    }

let updateBestTeams bestTeams model = {model with BestTeams = bestTeams}
let withClearChartErrorMessage model = { model with ChartsErrorStatus = None }

let withClearStatus model = { model with Status = None }

let teamAnsweredColumnChanged column model = 
        {model with SheetOptions = {model.SheetOptions with TeamAnswered = column}}

let validateTeamId gameDay teamIdString =
    
    let findTeamById teamId =
        gameDay
        |> GameDay.teams
        |> Seq.tryFind (fun team -> team.ID = teamId)
        |> Result.ofOption "Team not found"
    
    teamIdString
    |> PositiveNum.ofString
    |> Result.bind findTeamById
    

let rightAnswersColumnChanged column model = 
        {model with SheetOptions = {model.SheetOptions with Answered = column}}

let placesColumnChanged column model = 
        {model with SheetOptions = {model.SheetOptions with Place = column}}

let distanceColumnChanged column model = 
        {model with SheetOptions = {model.SheetOptions with Distance = column}}

let firstQuestionRowChanged row model = 
        {model with SheetOptions = {model.SheetOptions with Distance = row}}
    

let writeToSpreadSheetButtonAvailable window = 
    
    result{
        let! spreadsheetId = window.SpreadSheetId |> NoEmptyString.ofString
        let! sheetName = window.SheetName |> NoEmptyString.ofString
        let! teamId = window.TeamId |> PositiveNum.ofString
        
        let teamOption = 
            window.GameDay |> GameDay.teams |> Seq.tryFind(fun team -> team.ID = teamId)
        
        return! 
            match teamOption with
            | Some team -> 
                team
                |> Program.teamGameDay window.GameDay 
                |> Ok
            | None -> Error (sprintf "Team with ID %d not found" <| PositiveNum.value teamId) 
    }

let validateTeamIds (gameDay : GameDay) teamIds = 
        
    let findTeam teamId = 
        let teamOption = 
            gameDay
            |> GameDay.teams
            |> Seq.tryFind(fun t -> t.ID = teamId)
    
        match teamOption with 
        | Some team -> Result.Ok team
        | None -> Error "Team not found"

    teamIds
    |> String.splitByChar [|';'; ' '|]
    |> Array.filter (fun str -> match NoEmptyString.ofString str with Ok _ -> true | Error _ -> false)
    |> Array.map PositiveNum.ofString
    |> Array.map (Result.bind findTeam)
    |> Result.OfSeq (Result.Ok Seq.empty)
    |> Result.map List.ofSeq
    
let validateBestTeams input =
    match input with
    | "" -> Result.Ok None
    | x ->
        x
        |> PositiveNum.ofString
        |> Result.map (fun num -> Some num)
    

        
    
let showCharts input gameDay =
    
    let teamsToShow =
        
        match input with
        | CustomTeamsOnly customTeams -> customTeams |> Seq.ofList
        | BestTeamsOnly bestTeams -> gameDay |> GameDay.leadingTeams bestTeams 
        | CustomTeamsAndBestTeams (customTeams, bestTeams) ->  
            
            gameDay
            |> GameDay.leadingTeams bestTeams 
            |> Seq.append customTeams
            |> Seq.distinct
        
    
    Program.showPointsQuestionByQuestion gameDay teamsToShow
    Program.showPlacesQuestionByQuestion gameDay teamsToShow

type Message =
    | CustomTeamsEntered of string
    | BestTeamsCountEntered of string
    | ShowCharts of ShowChartsInput 
    | GoogleSpreadsheetCloseRequested
    
    | TeamIdEntered of team : string
    | SpreadsheetIdEntered of url : string
    | SpreadsheetNameEntered of sheetId : string
    | TeamAnsweredColumnChanged of newValue : string
    | RightAnswersColumnChanged of newValue : string
    | PlacesColumnChanged of newValue : string
    | DistanceColumnChanged of newValue : string
    | FirstQuestionColumnChanged of newValue : string
    | WriteToSpreadsheet of data : DataToWrite
    

let update message model = 
    match message with
    | CustomTeamsEntered customTeams -> {model with TeamIds = customTeams} |> withClearChartErrorMessage
    | BestTeamsCountEntered bestTeamsCount -> updateBestTeams bestTeamsCount model |> withClearChartErrorMessage
    | ShowCharts input -> 
        
        model.GameDay
        |> showCharts input

        model

    | TeamIdEntered teamId -> {model with TeamId = teamId}
        
    | SpreadsheetIdEntered url -> 
        
        let spreadSheetId = 
            
            // TODO Regex
            url 
            |> String.replace "https://docs.google.com/spreadsheets/d/" ""
            |> String.splitByChar [|'/'|]
            |> Array.head

        {model with SpreadSheetId = spreadSheetId}
        |> withClearStatus
            
    | SpreadsheetNameEntered sheetName -> {model with SheetName = sheetName; Status = None}
    | TeamAnsweredColumnChanged newValue -> model |> teamAnsweredColumnChanged newValue |> withClearStatus
    | RightAnswersColumnChanged newValue -> model |> rightAnswersColumnChanged newValue |> withClearStatus
    | PlacesColumnChanged newValue -> model |> placesColumnChanged newValue |> withClearStatus
    | DistanceColumnChanged newValue -> model |>  distanceColumnChanged newValue |> withClearStatus
    | FirstQuestionColumnChanged newValue -> {model with SheetOptions = {model.SheetOptions with FirstQuestion = int newValue}}
        
    | WriteToSpreadsheet data  -> 
        
        let result = 
            data
            |> SpreadsheetWriter.write model.SheetOptions model.SpreadSheetId model.SheetName
            |> Async.RunSynchronously
        
        let status = 
            result 
            |> Result.map (fun _ -> "Data is written")
            |> Some

        {model with Status = status}
    
    | GoogleSpreadsheetCloseRequested -> model
    

let bindings (wrap : Message -> 'a) = 
    (fun () -> [
        "TeamIds" |> Binding.twoWay ((fun m -> m.TeamIds), CustomTeamsEntered >> wrap)
        "BestTeams" |> Binding.twoWay((fun m -> m.BestTeams), BestTeamsCountEntered >> wrap)
        
        "ShowCharts" |> Binding.cmdIf
                (fun model ->
                    
                     let message = 
                         result {
                            let! customTeams = validateTeamIds model.GameDay model.TeamIds
                            let! bestTeams = validateBestTeams model.BestTeams
                            
                            let message = 
                                match bestTeams, customTeams with
                                | Some num, [] -> Ok <| BestTeamsOnly num
                                | Some num, x -> Ok <| CustomTeamsAndBestTeams(x, num)
                                | None, [] -> Error "Team ID or best teams count required"
                                | None, x -> Ok <| CustomTeamsOnly x
                            
                            return! message 
                            }
                     message
                     |> Result.map (fun data -> ShowCharts(data) |> wrap)
                 )
        "ChartsErrorMessageVisibility" |> Binding.oneWay(fun model -> model.ChartsErrorStatus |> Option.isSome)
        "ChartsErrorMessage" |> Binding.oneWay(fun model -> model.ChartsErrorStatus |> Option.defaultValue "")
                
        
        "TeamID" |> Binding.twoWayValidate(
            (fun m -> m.TeamId),
            TeamIdEntered >> wrap,
            (fun m -> validateTeamId m.GameDay m.TeamId)
            )
                
        "SpreadsheetID" |> Binding.twoWayValidate(
            (fun m -> m.SpreadSheetId),
            SpreadsheetIdEntered >> wrap,
            (fun m -> m.SpreadSheetId |> NoEmptyString.ofString)
            )

        "SheetName" |> Binding.twoWayValidate(
            (fun m -> m.SheetName),
            SpreadsheetNameEntered >> wrap,
            (fun m -> m.SheetName |> NoEmptyString.ofString)
            )

        "TeamAnsweredColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.TeamAnswered),
            TeamAnsweredColumnChanged >> wrap
            )
        "RightAnswersColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Answered),
            RightAnswersColumnChanged >> wrap
            )
        "PlacesColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Place),
            PlacesColumnChanged >> wrap
            )
        "DistanceColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Distance),
            DistanceColumnChanged >> wrap
            )
        "FirstQuestionColumn" |> Binding.twoWayValidate(
            (fun model -> model.SheetOptions.FirstQuestion |> string),
            FirstQuestionColumnChanged >> wrap,
            (fun model -> model.SheetOptions.FirstQuestion |> PositiveNum.ofInt)
            )
        
        "WriteToSpreadsheet" 
            |> Binding.cmdIf(fun model -> 
                                model 
                                |> writeToSpreadSheetButtonAvailable 
                                |> Result.map (fun data -> wrap(WriteToSpreadsheet data)))
        "ErrorMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> Option.map (function Error e -> true | _ -> false) |> Option.defaultValue false)
        "SuccessMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> Option.map (function Ok _ -> true | _ -> false) |> Option.defaultValue false)
        "StatusMessage" |> Binding.oneWay(fun model -> 
                                                model.Status 
                                                |> Option.map(function Ok m -> m | Error e -> e) 
                                                |> Option.defaultValue "")
    ])