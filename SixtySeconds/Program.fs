open Config
open Domain 
open Utils
open OutputParams
open SpreadsheetService
open GoogleChartService


type WriteMode = 
    | ReadOnly 
    | ReadAndWrite of string

type TeamChartMode = 
    | Show 

type CommandLineOption = 
    {
        SheetId : string option
        WriteMode : WriteMode
        TeamChart : TeamChartMode option
    }

let writeToSpreadsheet sheetName outputParams = 
    
    let google = Config.GetSample().Google
    let service = SpreadsheetService.getService ServiceMode.ReadWrite
    let firstRow = google.SheetRows.FirstQuestion

    

    let getRange column valuesCount = 
                
        sprintf "%s!%s%d:%s%d" sheetName column firstRow column <| firstRow + valuesCount

    let createValueRange column values = 
        
        let range = getRange column <| Seq.length values
        SpreadsheetService.createValueRange range MajorDimension.Column values

    let teamAnswered, rightAnsweredOn, places, distance = 
        outputParams.TeamAnswers |> List.map (Answer.isRight >> (fun b -> if b then "'+" else "")),
        outputParams.RightAnswersOn |> List.map string,
        outputParams.Places |> List.map ((fun p -> PositiveNum.value p.From, PositiveNum.value p.To) >> (fun (a, b) -> sprintf "%d-%d" a b)),
        outputParams.Distance |> List.map string

    let update range = 
        match SpreadsheetService.updateRequest service google.SpreadsheetId range with 
        | SUCCESS -> printfn "Written in %A" range.Range
        | ERROR x -> printfn "Error: %s" x

    [
        // пишем, ответили ли мы
        (google.SheetColumns.TeamAnswered, teamAnswered)
        // пишем число ответивших на вопрос
        (google.SheetColumns.Answered, rightAnsweredOn)
        // пишем место
        (google.SheetColumns.Place, places)
        // пишем отставание
        (google.SheetColumns.Distance, distance)
    ]
    |> List.map (fun t -> createValueRange <| fst t <| snd t)
    |> List.iter update

// Откуда читаем -read 06.05. 
// Куда пишем -write 06.05.2019
// Показывать график -show
// Показать и сохранить график -saveGraphic "06.05."

let rec parseCommandLine argv optionsSoFar = 
    
    match argv with 
    | [] -> optionsSoFar
        
    | "-read" :: sheetInput :: tail -> 
        
        let newOptions = {optionsSoFar with SheetId = Some sheetInput}
        parseCommandLine tail newOptions

    | "-write" :: sheetOutput :: tail -> 
        
        parseCommandLine tail {optionsSoFar with WriteMode = sheetOutput |> ReadAndWrite}
    
    | "-show" :: tail ->
        
        parseCommandLine tail {optionsSoFar with TeamChart = Some <| Show}

    | x::xs -> failwithf "Option '%s' is unrecognized" x


[<EntryPoint>]
let main argv = 
    
    let options = 
        let argsList = argv |> List.ofArray
        let defaultOptions = 
            {
                SheetId = None
                WriteMode = ReadOnly
                TeamChart = None
            }
        parseCommandLine argsList defaultOptions

    match options.SheetId with 
    | None -> failwithf "Missing key -read"
    | Some sheetInput ->
        let sixtySeconds = Config.GetSample().SixtySeconds

        let teams, gameDay = 
            
            sixtySeconds.PubHtml
            |> Parser.parse sheetInput

        let myTeamId = Config.GetSample().SixtySeconds.TeamId |> PositiveNum.ofInt
        let myTeam = teams |> Seq.find (fun t -> t.ID = myTeamId)

        let outParams = outputParams myTeam gameDay

        match options.WriteMode with 
        | ReadAndWrite sheetId -> writeToSpreadsheet sheetId outParams
        | _ -> ()

        match options.TeamChart with 
        | Some v -> 
            match v with 
            | Show -> 
                
                let teams = 
                    GameDay.getTopNTeams gameDay 1
                    |> Seq.append [myTeam]
                
                [Places; RightAnswers]
                |> Seq.iter (GoogleChart.showGraphic gameDay teams)
        | None -> ()
        

        printfn "%A"  myTeam
    0 // return an integer exit code
