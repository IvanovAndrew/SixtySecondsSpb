module SpreadsheetService

open Google.Apis.Auth.OAuth2
open Google.Apis.Sheets.v4
open Google.Apis.Sheets.v4.Data
open Google.Apis.Services
open Google.Apis.Util.Store

open System
open System.IO
open System.Threading

type Status = 
    | SUCCESS
    | ERROR of string

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

let getService mode = 
    
    let scopes = 
        match mode with
        | ReadOnly -> [| SheetsService.Scope.SpreadsheetsReadonly |]
        | ReadWrite -> [| SheetsService.Scope.Spreadsheets |]

    let applicationName = "Google Sheets API .NET Quickstart"
    let credPath = Path.Combine(Environment.CurrentDirectory, 
                                "../.credentials/sheets.googleapis.com-dotnet-quickstart.json")
    use stream = new FileStream("credentials.json", FileMode.Open, FileAccess.Read)
    let credential = GoogleWebAuthorizationBroker.AuthorizeAsync(
                        GoogleClientSecrets.Load(stream).Secrets,
                        scopes,
                        "user",
                        CancellationToken.None,
                        new FileDataStore(credPath, true)).Result

    let baseService = new BaseClientService.Initializer()
    baseService.ApplicationName <- applicationName
    baseService.HttpClientInitializer <- credential
    new SheetsService(baseService)

let createValueRange range majorDimension values = 
        
    let valueRange = new ValueRange()
        
    match majorDimension with
        | Row -> valueRange.MajorDimension <- "ROWS"
        | Column -> valueRange.MajorDimension <- "COLUMNS"
        
    valueRange.Range <- range
    valueRange.Values <- [| values |> Seq.map (fun v -> v :> obj) |> Array.ofSeq |]
    valueRange


let readRequest (service : SheetsService) spreadSheetID range = 
    
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

    let getRequest = service.Spreadsheets.Values.Get(spreadSheetID, range)
    getRequest.Execute().Values
    |> Seq.map toRange


let updateRequest (service : SheetsService) spreadsheetID valueRange = 

    let updateRequest = 
        let request = service.Spreadsheets.Values.Update(valueRange, spreadsheetID, valueRange.Range)
        request.ValueInputOption <- 
            SpreadsheetsResource.ValuesResource.UpdateRequest.ValueInputOptionEnum.USERENTERED 
            |> Some 
            |> Option.toNullable
        request

    let updateResponse = updateRequest.Execute()

    match updateResponse.UpdatedRows |> Option.ofNullable with
    | Some value when value > 0 -> SUCCESS
    | _ -> ERROR "No one rows are updated"