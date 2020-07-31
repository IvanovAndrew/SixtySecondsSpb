﻿module GameDayApp

open System.Windows

open SixtySeconds.Common.Errors
open SixtySeconds.Domain
open SixtySeconds.Actions
open Elmish.WPF

open SixtySeconds.Common.CommonTypes
open SixtySeconds.Infrastructure
open SixtySeconds.Settings
open SpreadsheetWriter

open SixtySeconds.Common.Errors
open SixtySeconds.Common.ErrorMessages

let private fromAsyncResult v = v |> Async.RunSynchronously |> Result.valueOrException

type TeamPerformance =
    {
        BestPlace : string
        BestPlaceAfterQuestion : int
        WorstPlace : string
        WorstPlaceAfterQuestion : int
        BestStrike : string
        WorstStrike : string
        DifficultAnsweredQuestion : int
        DifficultAnsweredQuestionCount : int
        SimplestWrongAnsweredQuestion : int
        SimplestWrongAnsweredQuestionCount : int
    }

    
let teamName (team : Team) = team.Name.Value
let teamPerformance (gameDay, team) =
    
    async {
        let! bestPlace = SixtySecondsApi.teamBestPlace (team, gameDay)
        let! bestPlaceQuestion = SixtySecondsApi.teamBestQuestion (team, gameDay)
        let! worstPlace  = SixtySecondsApi.teamWorstPlace (team, gameDay)
        let! worstPlaceQuestion = SixtySecondsApi.teamWorstQuestion (team, gameDay)
        let! bestStrike = SixtySecondsApi.teamBestStrike (team, gameDay)
        let! worstStrike = SixtySecondsApi.teamWorstStrike (team, gameDay)
        let! difficultAnsweredQuestion = SixtySecondsApi.teamDifficultAnsweredQuestion (team, gameDay)
        let! difficultAnsweredQuestionCount = SixtySecondsApi.teamDifficultAnsweredQuestionCount (team, gameDay)
        let! simplestWrongAnsweredQuestion = SixtySecondsApi.teamSimpleWrongAnsweredQuestion (team, gameDay)
        let! simplestWrongAnsweredQuestionCount = SixtySecondsApi.teamSimpleWrongAnsweredQuestionCount (team, gameDay)
        
        let teamPerformance =
            result {
                
                let! bp = bestPlace
                let! bpq = bestPlaceQuestion
                let! wp = worstPlace
                let! wpq = worstPlaceQuestion
                let! bs = bestStrike
                let! ws = worstStrike
                let! daq = difficultAnsweredQuestion
                let! daqc = difficultAnsweredQuestionCount
                let! swaq = simplestWrongAnsweredQuestion
                let! swaqc = simplestWrongAnsweredQuestionCount
                
                return {
                    BestPlace = Place.toString bp
                    BestPlaceAfterQuestion = bpq 
                    WorstPlace = Place.toString wp
                    WorstPlaceAfterQuestion = wpq
                    BestStrike = bs.Count |> Option.map (fun pn -> pn |> PositiveNum.value |> string) |> Option.defaultValue ""
                    WorstStrike = ws.Count |> Option.map (fun pn -> pn |> PositiveNum.value |> string) |> Option.defaultValue ""
                    DifficultAnsweredQuestion = daq
                    DifficultAnsweredQuestionCount = daqc
                    SimplestWrongAnsweredQuestion = swaq 
                    SimplestWrongAnsweredQuestionCount = swaqc 
                }
            }
        
        return teamPerformance
    }
    

type Model = 
    {
        GameDay : GameDay
        RatingType : RatingType
        Rating : GameDayRating
        QuestionsCount : int
        
        ChartTeamIds : string
        BestTeams : string
        ChartsErrorStatus : string option
        TeamId : string
        SpreadSheetId : string
        SheetName : string
        SheetOptions : SheetOptions
        Status : Result<string, string> option
        
        SelectedTeam : Team option
        SelectedTeamPerformance : TeamPerformance option
    }
    
type ShowChartsInput =
    | CustomTeamsOnly of Team list
    | BestTeamsOnly of PositiveNum
    | CustomTeamsAndBestTeams of teams : Team list * bestTeams : PositiveNum
    
type ChartType =
    | Answers of ShowChartsInput
    | Places of ShowChartsInput

let questionsRating gameDay =
    gameDay
    |> GameDay.allQuestions
    |> Seq.map (Question.rightAnswers gameDay)

let minThresholdValue = questionsRating >> Seq.min
let maxThresholdValue = questionsRating >> Seq.max

let updateRating ratingType gameDay =
    
    let result = 
        async {
            return! SixtySecondsApi.gameDayRating (gameDay, ratingType)
        } 
        
    result |> fromAsyncResult
    
let defaultSheetOptions = 
        {
            FirstQuestion = Config.load FirstQuestion |> int
            TeamAnswered = Config.load TeamAnswered
            Answered = Config.load RightAnswers
            Place = Config.load Place
            Distance = Config.load Distance
        }
        
let init gameDay =
    {
        GameDay = gameDay
        RatingType = All
        QuestionsCount = gameDay.PackageSize |> PositiveNum.value
        Rating = updateRating All gameDay 
        ChartTeamIds = ""
        BestTeams = ""
        ChartsErrorStatus = None
        SpreadSheetId = Config.load SpreadsheetUrl
        SheetName = gameDay.Name.Value
        TeamId = ""
        SheetOptions = defaultSheetOptions
        Status = None
        
        SelectedTeam = None
        SelectedTeamPerformance = None
    }
    

type CmdMsg =
    | UpdateTeamPerformance of team : Team * gameDay : GameDay
    | WriteToSpreadsheet of data : DataToWrite * Model  
    

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
    

let writeToSpreadSheetButtonAvailable model = 
    
    result{
        let! spreadsheetId = model.SpreadSheetId |> NoEmptyString.ofString
        let! sheetName = model.SheetName |> NoEmptyString.ofString
        let! teamId = model.TeamId |> PositiveNum.ofString
        
        let teamOption = 
            model.GameDay |> GameDay.teams |> Seq.tryFind(fun team -> team.ID = teamId)
        
        return!
            teamOption
            |> Result.ofOption (sprintf "Team with ID %d not found" <| PositiveNum.value teamId)
            |> Result.map (DataToWrite.fromGameDay model.GameDay) 
    }

let validateTeamIds (gameDay : GameDay) teamIds = 
        
    let findTeam teamId = 
            gameDay
            |> GameDay.teams
            |> Seq.tryFind(fun t -> t.ID = teamId)
            |> Result.ofOption "Team not found"
    
    teamIds
    |> String.splitByChar [|';'; ' '|]
    |> Array.filter (fun str -> match NoEmptyString.ofString str with Ok _ -> true | Error _ -> false)
    |> Array.map PositiveNum.ofString
    |> Array.map (Result.bind findTeam)
    |> Result.combine
    |> Result.map List.ofSeq
    
let validateBestTeams input =
    match input with
    | "" -> Result.Ok None
    | x ->
        x
        |> PositiveNum.ofString
        |> Result.map (fun num -> Some num)
    
let showChartButtonAvailable model =
    result {
        let! customTeams = validateTeamIds model.GameDay model.ChartTeamIds
        let! bestTeams = validateBestTeams model.BestTeams

        let message = 
            match bestTeams, customTeams with
            | Some num, [] -> Ok <| BestTeamsOnly num
            | Some num, x -> Ok <| CustomTeamsAndBestTeams(x, num)
            | None, x :: xs -> Ok <| CustomTeamsOnly customTeams
            | None, [] -> Error "Team ID or best teams count required"
            
        return! message 
    }
        
    
let showChart chartType gameDay =
    
    let teamsToShow input =
        
        match input with
        | CustomTeamsOnly customTeams -> customTeams |> Seq.ofList
        | BestTeamsOnly bestTeams -> gameDay |> Rating.ofGameDay |> Rating.leadingTeams bestTeams 
        | CustomTeamsAndBestTeams (customTeams, bestTeams) ->  
            
            gameDay
            |> Rating.ofGameDay
            |> Rating.leadingTeams bestTeams 
            |> Seq.append customTeams
            |> Seq.distinct
        
    match chartType with
    | Answers options ->
        options
        |> teamsToShow
        |> Chart.showPointsQuestionByQuestion gameDay
        
    | Places options ->
        options
        |> teamsToShow
        |> Chart.showPlacesQuestionByQuestion gameDay
        
        

        
let showErrorMessage status =

    status
    |> Option.map (fun res -> match res with Ok _ -> Visibility.Collapsed | Error _ -> Visibility.Visible)
    |> Option.defaultValue Visibility.Collapsed

let showSuccessMessage status =
    status
    |> Option.map (fun res -> match res with Ok _ -> Visibility.Visible | Error _ -> Visibility.Collapsed)
    |> Option.defaultValue Visibility.Collapsed

let saveOptions spreadsheet (options : SheetOptions) =
    
    let saveIfNotNull setting value =
        if value |> String.isEmpty |> not then Config.save setting value
        
    Config.save SpreadsheetUrl spreadsheet
    Config.save FirstQuestion options.FirstQuestion
    
    [
        TeamAnswered, options.TeamAnswered 
        RightAnswers, options.Answered 
        Place, options.Place 
        Distance, options.Distance 
    ]
    |> List.iter (fun (setting, value) -> saveIfNotNull setting value) 
    
type Message =
    | CustomTeamsEntered of string
    | BestTeamsCountEntered of string
    | ShowChart of ChartType  
    
    | RatingTypeChanged of bool 
    | QuestionThresholdChanged of int<RightAnswer>
    | UpdateRatingTable
    
    | TeamIdEntered of team : string
    | SpreadsheetIdEntered of url : string
    | SpreadsheetNameEntered of sheetId : string
    | TeamAnsweredColumnChanged of newValue : string
    | RightAnswersColumnChanged of newValue : string
    | PlacesColumnChanged of newValue : string
    | DistanceColumnChanged of newValue : string
    | FirstQuestionColumnChanged of newValue : string
    
    | WriteToSpreadsheetRequested of data : DataToWrite
    | DataWritten
    | DataNotWritten of SixtySecondsError 
    
    | TeamSelected of PositiveNum option
    | TeamPerformanceUpdated of TeamPerformance
    | TeamPerformanceFailed of SixtySecondsError
    

let update message model = 
    match message with
    | RatingTypeChanged filter ->
        let rating =
            if filter then 
                let teamsCount = 
                    model.GameDay
                    |> GameDay.teams
                    |> Seq.length
                teamsCount / 2
                |> Converter.rightAnswerFromInt
                |> Threshold
            else All
                    
        {model with RatingType = rating}, []
    | QuestionThresholdChanged threshold -> { model with RatingType = Threshold threshold}, []
        
    | UpdateRatingTable ->
        
        let newQuestionsCount =
            match model.RatingType with
            | All -> model.GameDay.PackageSize.Value
            | Threshold threshold -> 
                model.GameDay.PackageSize
                |> PositiveNum.createNaturalRange
                |> Seq.filter (fun q ->
                        let ra = q |> Question.rightAnswers model.GameDay
                        ra <= threshold)
                |> Seq.length
        {model with Rating = updateRating model.RatingType model.GameDay; QuestionsCount = newQuestionsCount}, []
        
    | CustomTeamsEntered customTeams -> {model with ChartTeamIds = customTeams} |> withClearChartErrorMessage, []
    | BestTeamsCountEntered bestTeamsCount -> updateBestTeams bestTeamsCount model |> withClearChartErrorMessage, []
    | ShowChart input -> 
        
        model.GameDay
        |> showChart input

        model, []
    | TeamSelected teamId ->
        
        let selectedTeam =
            
            let findTeam id = 
                model.GameDay
                |> GameDay.teams
                |> Seq.filter (fun team -> team.ID = id)
                |> Seq.tryHead
            
            teamId
            |> Option.bind findTeam
            
        let cmd =
            match selectedTeam with
            | Some team -> [UpdateTeamPerformance(team, model.GameDay)]
            | None -> []
            
        { model with SelectedTeam = selectedTeam}, cmd
    | TeamPerformanceUpdated teamPerformance -> {model with SelectedTeamPerformance = Some teamPerformance}, []
    | TeamPerformanceFailed error ->
        let errorMessage = error |> errorToString
        {model with Status = Some <| Error (errorMessage)}, [] 

    | TeamIdEntered teamId -> {model with TeamId = teamId}, []
        
    | SpreadsheetIdEntered url -> 
        
        let spreadSheetId = 
            
            // TODO Regex
            url 
            |> String.replace "https://docs.google.com/spreadsheets/d/" ""
            |> String.splitByChar [|'/'|]
            |> Array.head

        let nextState = {model with SpreadSheetId = spreadSheetId} |> withClearStatus
        nextState, []
            
    | SpreadsheetNameEntered sheetName -> {model with SheetName = sheetName} |> withClearStatus, []
    | TeamAnsweredColumnChanged newValue -> model |> teamAnsweredColumnChanged newValue |> withClearStatus, []
    | RightAnswersColumnChanged newValue -> model |> rightAnswersColumnChanged newValue |> withClearStatus, []
    | PlacesColumnChanged newValue -> model |> placesColumnChanged newValue |> withClearStatus, []
    | DistanceColumnChanged newValue -> model |>  distanceColumnChanged newValue |> withClearStatus, []
    | FirstQuestionColumnChanged newValue -> {model with SheetOptions = {model.SheetOptions with FirstQuestion = int newValue}} ,[]
        
    | WriteToSpreadsheetRequested data -> model, [WriteToSpreadsheet(data, model)]
    | DataWritten ->
        
        saveOptions model.SpreadSheetId model.SheetOptions
        {model with Status = "Data written" |> Ok |> Some}, []
        
    | DataNotWritten e ->
        
        {model with Status = e |> errorToString |> Error |> Some}, []
    
let bindings() = 
    [
        "FilterQuestions" |> Binding.twoWay(
                                             (fun m -> match m.RatingType with Threshold _ -> true | _ -> false),
                                              RatingTypeChanged
                                         )
        
        "ThresholdVisibility" |> Binding.oneWay (fun m -> match m.RatingType with Threshold _ -> Visibility.Visible | _ -> Visibility.Collapsed)
            
        "GameDayTable" |> Binding.subModelSeq(
                             (fun m -> m.Rating),
                             (fun (team, _, _) -> team.ID),
                             (fun() -> [
                                    "TeamName" |> Binding.oneWay (fun (_, (team, _, _)) -> team.Name.Value)
                                    "Rating" |> Binding.oneWay (fun (_, (_, rating, _)) -> rating)
                                    "Place" |> Binding.oneWay (fun (_, (_, _, place)) -> place |> Place.toString)
                                    ]
                             )
                                )
        "QuestionThreshold" |> Binding.twoWay(
            (fun m -> (match m.RatingType with All -> maxThresholdValue m.GameDay | Threshold t -> t) |> Converter.toInt |> float ),
            int >> Converter.rightAnswerFromInt >> QuestionThresholdChanged
        )
        
        "MinQuestionThreshold" |> Binding.oneWay(fun m -> minThresholdValue m.GameDay |> int |> float)
        "MaxQuestionThreshold" |> Binding.oneWay(fun m -> maxThresholdValue m.GameDay |> int |> float)
        
        "QuestionsCount" |> Binding.oneWay (fun m -> m.QuestionsCount)
        
        "ShowGameDayTable" |> Binding.cmd UpdateRatingTable
        
        "SelectionChanged" |> Binding.subModelSelectedItem(
                                                              "GameDayTable",
                                                              (fun m -> m.SelectedTeam |> Option.map (fun team -> team.ID)),
                                                              TeamSelected
                                                          )
        
        "TeamGameDayInfoVisibility" |> Binding.oneWay (fun m -> match m.SelectedTeam with Some _ -> Visibility.Visible | _ -> Visibility.Hidden)
        "TeamName" |> Binding.oneWay (fun m -> m.SelectedTeam |> Option.map teamName |> Option.defaultValue "")
        "TeamIDChosen" |> Binding.oneWay (fun m -> m.SelectedTeam |> Option.map (fun team -> team.ID |> PositiveNum.value) |> Option.defaultValue 0)
        "BestPlace" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.BestPlace) |> Option.defaultValue "")
        "BestPlaceQuestion" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.BestPlaceAfterQuestion) |> Option.defaultValue 0)
        "WorstPlace" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.WorstPlace) |> Option.defaultValue "")
        "WorstPlaceQuestion" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.WorstPlaceAfterQuestion) |> Option.defaultValue 0)
        "BestStrike" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.BestStrike) |> Option.defaultValue "")
        "WorstStrike" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.WorstStrike) |> Option.defaultValue "")
        "DifficultAnsweredQuestion" |> Binding.oneWay (
                                                          fun m -> m.SelectedTeamPerformance
                                                                   |> Option.map (fun tp -> tp.DifficultAnsweredQuestion)
                                                                   |> Option.defaultValue 0
                                                      )
        "DifficultAnsweredQuestionCount" |> Binding.oneWay (
                                                          fun m -> m.SelectedTeamPerformance
                                                                   |> Option.map (fun tp -> tp.DifficultAnsweredQuestionCount)
                                                                   |> Option.defaultValue 0
                                                      )
        "SimplestWrongAnsweredQuestion" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.SimplestWrongAnsweredQuestion) |> Option.defaultValue 0)
        "SimplestWrongAnsweredQuestionCount" |> Binding.oneWay (fun m -> m.SelectedTeamPerformance |> Option.map (fun tp -> tp.SimplestWrongAnsweredQuestionCount) |> Option.defaultValue 0)
        
        "ShowChartTitle" |> Binding.oneWay (fun m -> sprintf "Show chart for game %s" m.GameDay.Name.Value)
        "TeamIds" |> Binding.twoWayValidate(
                                                (fun m -> m.ChartTeamIds),
                                                CustomTeamsEntered,
                                                (fun m -> validateTeamIds m.GameDay m.ChartTeamIds)
                                            )
        "BestTeams" |> Binding.twoWayValidate
                    (
                        (fun m -> m.BestTeams),
                        BestTeamsCountEntered,
                        (fun m -> validateBestTeams m.BestTeams)
                    )
        
        "ShowPlacesCharts" |> Binding.cmdIf
                    (fun model ->
                         model
                         |> showChartButtonAvailable
                         |> Result.map (Places >> ShowChart)
                     )
        "ShowAnswersCharts" |> Binding.cmdIf
                    (fun model ->
                         model
                         |> showChartButtonAvailable
                         |> Result.map (Answers >> ShowChart)
                     )
        "ChartsErrorMessageVisibility" |> Binding.oneWay(fun model -> model.ChartsErrorStatus |> Option.isSome)
        "ChartsErrorMessage" |> Binding.oneWay(fun model -> model.ChartsErrorStatus |> Option.defaultValue "")
                
        "WriteToSpreadsheetTitle" |> Binding.oneWay(fun model -> model.GameDay.Name.Value |> sprintf "Write game %s to google spreadsheet")
        "TeamID" |> Binding.twoWayValidate
                    (
                        (fun m -> m.TeamId),
                        TeamIdEntered,
                        (fun m ->
                            let validate s =
                                if String.isEmpty s then Ok()
                                else
                                    s
                                    |> validateTeamId m.GameDay
                                    |> Result.map (fun _ -> ()) 
                                    
                            validate m.TeamId
                        )
                    )
                
        "SpreadsheetID" |> Binding.twoWay(
            (fun m -> m.SpreadSheetId),
            SpreadsheetIdEntered
            )

        "SheetName" |> Binding.twoWay(
            (fun m -> m.SheetName),
            SpreadsheetNameEntered
            )

        "TeamAnsweredColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.TeamAnswered),
            TeamAnsweredColumnChanged
            )
        "RightAnswersColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Answered),
            RightAnswersColumnChanged
            )
        "PlacesColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Place),
            PlacesColumnChanged
            )
        "DistanceColumn" |> Binding.twoWay(
            (fun model -> model.SheetOptions.Distance),
            DistanceColumnChanged
            )
        "FirstQuestionColumn" |> Binding.twoWayValidate(
            (fun model -> model.SheetOptions.FirstQuestion |> string),
            FirstQuestionColumnChanged,
            (fun model -> model.SheetOptions.FirstQuestion |> PositiveNum.ofInt)
            )
        
        "WriteToSpreadsheet" 
            |> Binding.cmdIf(fun model -> 
                                model 
                                |> writeToSpreadSheetButtonAvailable 
                                |> Result.map WriteToSpreadsheetRequested)
        "ErrorMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> showErrorMessage )
        "SuccessMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> showSuccessMessage)
        "StatusMessage" |> Binding.oneWay(fun model -> 
                                                model.Status 
                                                |> Option.map(function Ok m -> m | Error e -> e) 
                                                |> Option.defaultValue "")
    ]
    
let writeToSpreadsheet (data, model) =
    
    data
    |> SpreadsheetWriter.write model.SheetOptions model.SpreadSheetId model.SheetName
    
let toCmd cmdMsg (wrap : Message -> 'a) =
    match cmdMsg with
    | UpdateTeamPerformance (team, gameDay) ->
        
        let ofSuccess result =
            match result with
            | Ok performance -> performance |> TeamPerformanceUpdated |> wrap
            | Error e -> e |> TeamPerformanceFailed |> wrap
            
        let ofError exn = exn |> Bug |> TeamPerformanceFailed |> wrap
        
        Elmish.Cmd.OfAsync.either teamPerformance (gameDay, team) ofSuccess ofError
    
    | WriteToSpreadsheet(data, model) ->
        
        let ofSuccess result =
            match result with
            | Ok _ -> DataWritten |> wrap
            | Error e -> e |> Bug |> DataNotWritten |> wrap
        
        let ofError exn = exn |> Bug |> DataNotWritten |> wrap
        
        Elmish.Cmd.OfAsync.either writeToSpreadsheet (data, model) ofSuccess ofError