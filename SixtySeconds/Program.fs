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
    | Show of int

type CommandLineOption = 
    {
        SheetId : string option
        TeamId : PositiveNum
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
        outputParams.TeamAnswers |> List.map (function Right ->  "'+" | _ -> ""),
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
// Какая команда интересует -team 15
// Куда пишем -write 06.05.2019
// Показывать график -show

let rec parseCommandLine argv optionsSoFar = 
    
    match argv with 
    | [] -> optionsSoFar
        
    | "-read" :: sheetInput :: tail -> 
        
        let newOptions = {optionsSoFar with SheetId = Some sheetInput}
        parseCommandLine tail newOptions

    | "-team" :: team :: tail -> 
        
        parseCommandLine tail {optionsSoFar with TeamId = team |> int |> PositiveNum.ofInt}

    | "-write" :: sheetOutput :: tail -> 
        
        parseCommandLine tail {optionsSoFar with WriteMode = sheetOutput |> ReadAndWrite}
    
    | "-show" :: topN :: tail ->
        
        let options = {optionsSoFar with TeamChart = topN |> int |> Show |> Some }
        parseCommandLine tail options

    | x::xs -> failwithf "Option '%s' is unrecognized" x


[<EntryPoint>]
let main argv = 
    
    let options = 
        let argsList = argv |> List.ofArray
        let defaultOptions = 
            {
                SheetId = None
                TeamId = Config.GetSample().SixtySeconds.TeamId |> PositiveNum.ofInt
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

        let myTeam = teams |> Seq.tryFind (fun t -> t.ID = options.TeamId)

        match myTeam with 
        | Some team -> 

            let outParams = outputParams team gameDay

            match options.WriteMode with 
            | ReadAndWrite sheetId -> writeToSpreadsheet sheetId outParams
            | _ -> ()

            match options.TeamChart with 
            | Some v -> 
                match v with 
                | Show topN -> 
                
                    let teams = 
                        GameDay.getTopNTeams gameDay topN
                        |> Seq.rev
                        |> Seq.append [team]
                        |> Seq.distinct
                    

                    [Places; RightAnswers]
                    |> Seq.iter (GoogleChart.showGraphic gameDay teams)
            | None -> ()
        | None -> ()
        

        printfn "%A"  myTeam
    0 // return an integer exit code
