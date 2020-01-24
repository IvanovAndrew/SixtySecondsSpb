module SpreadsheetService

open Google.Apis.Auth.OAuth2
open Google.Apis.Sheets.v4
open Google.Apis.Sheets.v4.Data
open Google.Apis.Services
open Google.Apis.Util.Store

open System
open System.IO
open System.Threading
open Utils

type ServiceMode = 
    | ReadOnly
    | ReadWrite

type MajorDimension = 
    | Row
    | Column

type Row = 
    {
        Number : int
        Question : string option
    }

let AsyncGetService mode = 
    
    async {
        let scopes = 
            match mode with
            | ReadOnly -> [| SheetsService.Scope.SpreadsheetsReadonly |]
            | ReadWrite -> [| SheetsService.Scope.Spreadsheets |]

        let applicationName = "Sixty seconds info"
        let credPath = Path.Combine(Environment.CurrentDirectory, 
                                    "../.credentials/sheets.googleapis.com-dotnet-quickstart.json")
        use stream = new FileStream("credentials.json", FileMode.Open, FileAccess.Read)
        let! credential = GoogleWebAuthorizationBroker.AuthorizeAsync(
                            GoogleClientSecrets.Load(stream).Secrets,
                            scopes,
                            "user",
                            CancellationToken.None,
                            new FileDataStore(credPath, true))
                            |> Async.AwaitTask 

        let baseService = new BaseClientService.Initializer()
        baseService.ApplicationName <- applicationName
        baseService.HttpClientInitializer <- credential
        return new SheetsService(baseService)
    }
    

let createValueRange range majorDimension values = 
        
    let valueRange = new ValueRange()
        
    match majorDimension with
        | Row -> valueRange.MajorDimension <- "ROWS"
        | Column -> valueRange.MajorDimension <- "COLUMNS"
        
    valueRange.Range <- range
    valueRange.Values <- [| values |> Seq.map (fun v -> v :> obj) |> Array.ofSeq |]
    valueRange


let AsyncReadRequest (service : SheetsService) spreadSheetID range = 
    
    async {
        let toRange input = 
        
            let list = input |> List.ofSeq
        
            match list with 
            | first :: second :: [] -> 
                {
                    Number = first |> string |> int
                    Question = second |> string |> Some
                }
            | first :: [] -> 
                {
                    Number = first |> string |> int
                    Question = None
                }
            | _ -> failwith "Что-то пошло не так"

        let request = service.Spreadsheets.Values.Get(spreadSheetID, range)
        
        let! executionResult = request.ExecuteAsync() |> Async.AwaitTask

        let result = 
            executionResult.Values
            |> Seq.map toRange
        return result
    }


let AsyncUpdateRequest (service : SheetsService) spreadsheetID valueRange = 

    async {
        let updateRequest = 
            let request = service.Spreadsheets.Values.Update(valueRange, spreadsheetID, valueRange.Range)
            request.ValueInputOption <- 
                SpreadsheetsResource.ValuesResource.UpdateRequest.ValueInputOptionEnum.USERENTERED 
                |> Some 
                |> Option.toNullable
            request

        let! updateResponse = updateRequest.ExecuteAsync() |> Async.AwaitTask

        let result = 
            match updateResponse.UpdatedRows |> Option.ofNullable with
            | Some value when value > 0 -> Ok()
            | _ -> Error "No one rows are updated"
        
        return result
    }