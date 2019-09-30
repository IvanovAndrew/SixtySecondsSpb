open Config
open Domain 
open Utils
open SpreadsheetService
open GoogleChartService
open PositiveNum


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
        Total : PositiveNum option
    }

let writeToSpreadsheet sheetName outputParams = 
    
    let google = Config.GetSample().Google
    let service = 
        SpreadsheetService.AsyncGetService ServiceMode.ReadWrite 
        |> Async.RunSynchronously
    let firstRow = google.SheetRows.FirstQuestion

    let teamAnswers, rightAnsweredOn, places, distance = outputParams

    let getRange column valuesCount = 
        sprintf "%s!%s%d:%s%d" sheetName column firstRow column <| firstRow + valuesCount

    let createValueRange column values = 
        let range = getRange column <| Seq.length values
        SpreadsheetService.createValueRange range MajorDimension.Column values

    let update range = 
        
        let result = 
            range 
            |> SpreadsheetService.AsyncUpdateRequest service google.SpreadsheetId 
            |> Async.RunSynchronously

        match result with 
        | SUCCESS -> printfn "Written in %A" range.Range
        | ERROR x -> printfn "Error: %s" x

    [
        // пишем, ответили ли мы
        (google.SheetColumns.TeamAnswered, teamAnswers |> Seq.map (function Right ->  "'+" | _ -> "") |> Array.ofSeq)
        // пишем число ответивших на вопрос
        (google.SheetColumns.Answered, rightAnsweredOn |> Seq.map string |> Array.ofSeq)
        // пишем место
        (google.SheetColumns.Place, places |> Seq.map (fun p -> sprintf "%d-%d" <| PositiveNum.value p.From <| PositiveNum.value p.To) |> Array.ofSeq)
        // пишем отставание
        (google.SheetColumns.Distance, distance |> Seq.map string |> Array.ofSeq)
    ]
    |> List.map (fun t -> createValueRange <| fst t <| snd t)
    |> List.iter update

let showGraphic data teams gameDay vAxis = 
    
    let graphicData = 
        data 
        |> Seq.map (fun g -> g |> Seq.mapi (fun i v -> i+1, v))

    let labels = teams |> Seq.map (fun t -> t.Name |> NoEmptyString.value)

    let hMax = gameDay.QuestionsCount |> PositiveNum.value

    let options = 
        {
            HorizonalAxis = {Direction = Direction.Forward; Label = "Вопрос"; Maximum = hMax}
            VerticalAxis = vAxis
            Title = sprintf "Открытая Лига \"60 секунд\" %s" <| gameDay.Day.ToShortDateString()
        }

    GoogleChart.showData graphicData labels <| ChartType.Line options

let showPlacesQuestionByQuestion gameDay teams = 
    
    let places = 
        let places team = 
            gameDay.QuestionsCount 
            |> PositiveNum.createNaturalRange
            |> Seq.map (GameDay.getPlaceAfterQuestion gameDay team)
            |> Seq.map (fun p -> p.From |> PositiveNum.value)

        teams 
        |> Seq.map places
    
    let vMax = places |> Seq.map (Seq.max) |> Seq.max
    let verticalAxis = {Direction = Direction.Back; Label = "Места"; Maximum = vMax}

    showGraphic places teams gameDay verticalAxis


let showPointsQuestionByQuestion gameDay teams = 

    let rightAnswers = 
        let answers team = 
            gameDay.QuestionsCount
            |> PositiveNum.createNaturalRange 
            |> Seq.map (fun q -> GameDay.totalAnswered gameDay q team)

        teams 
        |> Seq.map answers
    
    let vMax = rightAnswers |> Seq.map (Seq.max) |> Seq.max

    let verticalAxis = {Direction = Forward; Label = "Правильные ответы"; Maximum = vMax}

    showGraphic rightAnswers teams gameDay verticalAxis


let showTotalTable url topN = 
        
    let data = url |> Parser.parseTotal

    let topNResultTable = 
        data
        |> SeasonTable.topNResult topN

    let columns = 
            
        let toColumnFormat table = 
            table 
            |> Seq.map (fun (team, points) -> team.Name |> NoEmptyString.value, points)

        [topNResultTable; data.Table]
        |> Seq.map toColumnFormat
        
    let labels = 
        [
            "Team"; 
            (sprintf "Best %d games" <| PositiveNum.value topN); 
            (sprintf "All %d games" <| PositiveNum.value data.GamesCount); 
        ]

    GoogleChart.showData columns labels ChartType.Table

let processGameDay options gameDay = 
    
    let myTeam = gameDay |> GameDay.teams |> Seq.tryFind (fun t -> t.ID = options.TeamId)

    match myTeam with 
    | Some team -> 

        let outParams = 
                
            let allQuestions = 
                PositiveNum.createNaturalRange gameDay.QuestionsCount

            let teamAnswered = 
                allQuestions
                |> Seq.map (GameDay.getAnswer gameDay team)

            let rightAnsweredOn = 
                allQuestions
                |> Seq.map (GameDay.rightAnswersOnQuestion gameDay)

            let places = 
                allQuestions
                |> Seq.map (GameDay.getPlaceAfterQuestion gameDay team)

            let distance = 
                allQuestions 
                |> Seq.map (GameDay.getDistanceFromTheFirstPlace gameDay team)

            teamAnswered, rightAnsweredOn, places, distance
                

        match options.WriteMode with 
        | ReadAndWrite sheetId -> writeToSpreadsheet sheetId outParams
        | _ -> ()

        let showTeamChart mode = 
                
            match mode with 
            | Show topN -> 
                
                let teams = 
                    [team]
                    |> Seq.append (GameDay.leadingTeams gameDay topN)
                    |> Seq.distinct

                showPlacesQuestionByQuestion gameDay teams
                showPointsQuestionByQuestion gameDay teams

        options.TeamChart
        |> Option.iter showTeamChart
            
    | None -> failwith "Team with Id %d not found" <| PositiveNum.value options.TeamId 


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

    | "-total" :: games :: tail -> 
        let options = {optionsSoFar with Total = games |> int |> PositiveNum.ofInt |> Some}
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
                Total = None
            }
        parseCommandLine argsList defaultOptions

    let sixtySeconds = Config.GetSample().SixtySeconds

    options.SheetId 
    |> Option.map (fun sheetInput -> sixtySeconds.PubHtml |> Parser.parse sheetInput)
    |> Option.iter (processGameDay options)
    

    options.Total
    |> Option.iter (showTotalTable sixtySeconds.PubHtml)
        
    0 // return an integer exit code