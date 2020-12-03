open Chart
open Config


open SixtySeconds.Common.CommonTypes

open SixtySeconds.Common.Errors
open SixtySeconds.Common.ErrorMessages

open SixtySeconds.Services

open SixtySeconds.Domain
open SixtySeconds.Actions
open SixtySeconds.SixtySecondsProgramBuilder
open Shared.Models


type WriteMode = 
    | ReadOnly 
    | ReadAndWrite of string

type TeamChartMode = 
    | Show of PositiveNum


type CommandLineOption = 
    {
        SheetId : NoEmptyString option
        TeamId : PositiveNum
        WriteMode : WriteMode
        TeamChart : TeamChartMode option
        Total : PositiveNum option
    }

let rec parseCommandLine argv optionsSoFar = 
    
    match optionsSoFar with 
    | Error e -> Error e 
    | Ok options -> 

        match argv with 
        | [] -> optionsSoFar
        
        | "-read" :: sheetInput :: tail -> 
            
            let newOptions =
                match NoEmptyString.ofString sheetInput with
                | Ok value -> Ok {options with SheetId = Some value}
                | Error e -> Error e
            parseCommandLine tail newOptions

        | "-team" :: team :: tail -> 
            
            let options =
                match team |> PositiveNum.ofString  with 
                | Ok teamId -> 
                    Ok {options with TeamId = teamId}
                | Error e -> Error e
            parseCommandLine tail options

        | "-write" :: sheetOutput :: tail -> 
            parseCommandLine tail <| Ok {options with WriteMode = sheetOutput |> ReadAndWrite}
    
        | "-show" :: topN :: tail ->
            topN |> PositiveNum.ofString 
            |> Result.bind (fun teams -> parseCommandLine tail <| Ok {options with TeamChart = teams |> Show |> Some})
            

        | "-total" :: games :: tail -> 
            
            let totalGamesResult = games |> PositiveNum.ofString
            match totalGamesResult with 
            | Error e -> Error e
            | Ok r -> 
                parseCommandLine tail <| Ok {options with Total = r |> Some}
            

        | x::xs -> Error <| sprintf "Option '%s' is unrecognized" x

let processGameDay options gameDay = 
    
    let myTeam = gameDay |> GameDay.teams |> Seq.tryFind (fun t -> t.ID = options.TeamId)

    match myTeam with 
    | Some team -> 

        let data = team |> DataToWrite.fromGameDay gameDay
                
        match options.WriteMode with 
        | ReadAndWrite sheetId -> 
            
            let google = Config.GetSample().Google
            
            let options = 
                
                {
                    Id = google.SpreadsheetId
                    SheetName = sheetId
                    ColumnOptions = {

                        FirstQuestion = google.SheetRows.FirstQuestion
                        TeamAnswered = google.SheetColumns.TeamAnswered
                        Answered = google.SheetColumns.Answered
                        Place = google.SheetColumns.Place
                        Distance = google.SheetColumns.Distance
                    }                    
                }

            data
            |> SpreadsheetWriter.write options   
            |> Async.RunSynchronously
            |> ignore
        | _ -> ()

        let showTeamChart mode = 
                
            match mode with 
            | Show topN -> 
                
                let teams = 
                    [team]
                    |> Seq.append (gameDay |> Rating.ofGameDay |> Rating.leadingTeams topN)
                    |> Seq.distinct

                showPlacesQuestionByQuestion gameDay teams
                showPointsQuestionByQuestion gameDay teams

        options.TeamChart
        |> Option.iter (fun c -> c |> showTeamChart |> Async.RunSynchronously)
            
    | None -> failwithf "Team with Id %d not found" options.TeamId.Value

[<EntryPoint>]
let main argv =
    
    let options = 
        let argsList = argv |> List.ofArray
        
        let result = 
            result{
                let! defaultTeamId = Config.GetSample().SixtySeconds.TeamId |> PositiveNum.ofInt 
            
                let defaultOptions = 
                    {
                        SheetId = None
                        TeamId = defaultTeamId
                        WriteMode = ReadOnly
                        TeamChart = None
                        Total = None
                    } |> Ok

                return! parseCommandLine argsList defaultOptions 
            }
        
        match result with 
        | Ok value -> value
        | Error e -> failwith e


    let sixtySeconds = Config.GetSample().SixtySeconds

    match options.SheetId with 
    | Some sheet -> 
        
        let res = 
            async {
                let! document =
                    let url = 
                        sixtySeconds.PubHtml
                        |> Url.create
                        
                    match url with
                    | Ok u -> Parser.asyncLoadDocument u
                    // TODO remove it
                    | Error e -> failwith e
                
                return
                    document
                    |> expectWebRequestError
                    |> Result.bind (fun d -> d |> Parser.parseGameday sheet |> expectParsingError)
                    |> Result.map (processGameDay options)
            } |> Async.RunSynchronously
        
        match res with 
        | Error e -> e |> errorToString |> failwith
        | Ok _ -> ()
    | None -> ()
    
    let seasonTableResult = 
        match options.Total with
        | Some gamesToCount -> 
            
            async {
                let! document =
                    let url = 
                        sixtySeconds.PubHtml
                        |> Url.create
                    
                    match url with
                    | Ok v -> Parser.asyncLoadDocument v
                    // TODO remove it
                    | Error e -> failwith "Wrong url"
                    
                return 
                    document
                    |> expectWebRequestError
                    |> Result.bind (fun v -> v |> Parser.parseTotalFromGoogleSpreadsheet |> expectParsingError)  
                    |> Result.map (fun seasonTable -> showTotalTable seasonTable gamesToCount |> Async.RunSynchronously)
            } |> Async.RunSynchronously
            
        | None -> Ok()
        
    0 // return an integer exit code