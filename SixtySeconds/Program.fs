open Config
open Domain 
open Utils
open SpreadsheetService
open GoogleChartService
open PositiveNum
open SpreadsheetWriter


type WriteMode = 
    | ReadOnly 
    | ReadAndWrite of string

type TeamChartMode = 
    | Show of PositiveNum

type CommandLineOption = 
    {
        SheetId : string option
        TeamId : PositiveNum
        WriteMode : WriteMode
        TeamChart : TeamChartMode option
        Total : PositiveNum option
    }
    

let teamGameDay gameDay team = 
    let allQuestions = 
        PositiveNum.createNaturalRange gameDay.PackageSize

    {
        TeamAnswers = allQuestions |> Seq.map (GameDay.getAnswer gameDay team)
        RightAnswersOn = allQuestions |> Seq.map (GameDay.rightAnswersOnQuestion gameDay)
        Places = allQuestions |> Seq.map (GameDay.getPlaceAfterQuestion gameDay team)
        Distance = allQuestions |> Seq.map (GameDay.getDistanceFromTheFirstPlace gameDay team)
    }

let showGraphic data teams gameDay vAxis = 
    
    let graphicData = 
        data 
        |> Seq.map (fun g -> g |> Seq.mapi (fun i v -> i+1, v))

    let labels = teams |> Seq.map (fun t -> t.Name |> NoEmptyString.value)

    let hMax = gameDay.PackageSize |> PositiveNum.value

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
            gameDay.PackageSize 
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
            gameDay.PackageSize
            |> PositiveNum.createNaturalRange 
            |> Seq.map (fun q -> GameDay.totalAnswered gameDay q team)

        teams 
        |> Seq.map answers
    
    let vMax = rightAnswers |> Seq.map (Seq.max) |> Seq.max

    let verticalAxis = {Direction = Forward; Label = "Правильные ответы"; Maximum = Converter.toInt vMax}

    showGraphic rightAnswers teams gameDay verticalAxis


let showTotalTable data topN = 
        
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

        let data = teamGameDay gameDay team
                
        match options.WriteMode with 
        | ReadAndWrite sheetId -> 
            
            let google = Config.GetSample().Google
            
            let spreadsheetId = google.SpreadsheetId

            let options = 
                
                
                {
                    FirstQuestion = google.SheetRows.FirstQuestion

                    TeamAnswered = google.SheetColumns.TeamAnswered
                    Answered = google.SheetColumns.Answered
                    Place = google.SheetColumns.Place
                    Distance = google.SheetColumns.Distance
                }

            data
            |> SpreadsheetWriter.write options spreadsheetId sheetId 
            |> Async.RunSynchronously
            |> ignore
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
    
    match optionsSoFar with 
    | Error e -> Error e 
    | Ok options -> 

        match argv with 
        | [] -> optionsSoFar
        
        | "-read" :: sheetInput :: tail -> 
            let newOptions = Ok {options with SheetId = Some sheetInput}
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
                    sixtySeconds.PubHtml 
                    |> Parser.asyncLoadDocument 

                return
                    document
                    |> Result.bind (Parser.parse sheet)
                    |> Result.map (processGameDay options)
            } |> Async.RunSynchronously
        
        match res with 
        | Error e -> failwith e
        | Ok _ -> ()
    | None -> ()
    
    let seasonTableResult = 
        match options.Total with
        | Some gamesToCount -> 
            
            async {
                let! document = sixtySeconds.PubHtml |> Parser.asyncLoadDocument

                return 
                    document
                    |> Result.bind Parser.parseTotal 
                    |> Result.map (fun seasonTable -> showTotalTable seasonTable gamesToCount)
            } |> Async.RunSynchronously

            
        | None -> Ok()
        
    0 // return an integer exit code