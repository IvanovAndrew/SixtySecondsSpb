module SpreadsheetApp

open System

open SpreadsheetWriter
open Utils

open Elmish
open Elmish.WPF

type Model = 
    {
        Data : DataToWrite
        SpreadSheetId : string
        SheetName : string
        SheetOptions : SheetOptions
        Status : Result<string, string> option
    }

type Message = 
    | SpreadsheetIdEntered of url : string
    | SpreadsheetNameEntered of sheetId : string
    | TeamAnsweredColumnChanged of newValue : string
    | RightAnswersColumnChanged of newValue : string
    | PlacesColumnChanged of newValue : string
    | DistanceColumnChanged of newValue : string
    | FirstQuestionColumnChanged of newValue : string
    | WriteToSpreadsheet
    
let init data = 
    
    let defaultSheetOptions = 
        {
            FirstQuestion = 3
            TeamAnswered = "E"
            Answered = "F"
            Place = "G"
            Distance = "H"
        }

    {
        Data = data; 
        SpreadSheetId = String.Empty; 
        SheetName = String.Empty; 
        SheetOptions = defaultSheetOptions
        Status = None
    }

let withClearStatus model = { model with Status = None }

let teamAnsweredColumnChanged column model = 
        {model with SheetOptions = {model.SheetOptions with TeamAnswered = column}}

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

        return Ok()
    }


let update msg m = 
    match msg with 
    | SpreadsheetIdEntered url -> 
        
        let spreadSheetId = 
            
            // TODO Regex
            url 
            |> String.replace "https://docs.google.com/spreadsheets/d/" ""
            |> String.splitByChar [|'/'|]
            |> Array.head

        {m with SpreadSheetId = spreadSheetId;}
        |> withClearStatus
            
    | SpreadsheetNameEntered sheetName -> {m with SheetName = sheetName; Status = None}
    | TeamAnsweredColumnChanged newValue -> m |> teamAnsweredColumnChanged newValue |> withClearStatus
    | RightAnswersColumnChanged newValue -> m |> rightAnswersColumnChanged newValue |> withClearStatus
    | PlacesColumnChanged newValue -> m |> placesColumnChanged newValue |> withClearStatus
    | DistanceColumnChanged newValue ->m |>  distanceColumnChanged newValue |> withClearStatus
    | FirstQuestionColumnChanged newValue -> {m with SheetOptions = {m.SheetOptions with FirstQuestion = int newValue}}
        
    | WriteToSpreadsheet -> 
        
        let result = 
            m.Data
            |> SpreadsheetWriter.write m.SheetOptions m.SpreadSheetId m.SheetName
            |> Async.RunSynchronously
        
        let status = 
            result 
            |> Result.map (fun _ -> "Data is written")
            |> Some

        {m with Status = status}

let bindings (wrap : Message -> 'a) = 
    (fun () -> [
        "SheetUrl" |> Binding.twoWayValidate(
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
            (fun model -> model.SheetOptions.FirstQuestion |> Ok)
            )
        
        "WriteToSpeadSheet" 
            |> Binding.cmdIf(fun model -> 
                                model 
                                |> writeToSpreadSheetButtonAvailable 
                                |> Result.map (fun _ -> wrap(WriteToSpreadsheet)))
        "ErrorMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> Option.map (function Error e -> true | _ -> false) |> Option.defaultValue false)
        "SuccessMessageVisibility" |> Binding.oneWay (fun m -> m.Status |> Option.map (function Ok _ -> true | _ -> false) |> Option.defaultValue false)
        "StatusMessage" |> Binding.oneWay(fun model -> 
                                                model.Status 
                                                |> Option.map(function Ok m -> m | Error e -> e) 
                                                |> Option.defaultValue "")
    ])