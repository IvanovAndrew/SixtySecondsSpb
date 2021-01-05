module Chart

open SixtySeconds.Domain
open SixtySeconds.Actions
open SixtySeconds.Services
open SixtySeconds.Services.GoogleChartService
open SixtySeconds.Common.CommonTypes


let showGraphic data (teams : Team seq) gameDay vAxis = 
    
    let horizontalAxisLabel = "Question"
    
    let graphicData = 
        data 
        |> Seq.map (fun g -> g |> Seq.mapi (fun i v -> i+1, v))

    let labels = teams |> Seq.map (fun t -> t.Name.Value)

    let hMax = gameDay.PackageSize.Value

    let options = 
        {
            HorizonalAxis = {Direction = Direction.Forward; Label = horizontalAxisLabel; Maximum = hMax}
            VerticalAxis = vAxis
            Title = sprintf "%s %s %s" gameDay.Tournament.City.Value gameDay.Tournament.League.Value gameDay.Name.Value
        }

    GoogleChart.showData graphicData labels <| ChartType.Line options
    
    
let showPlacesQuestionByQuestion gameDay teams = 
    
    let places = 
        let places team = 
            gameDay
            |> GameDay.allQuestions
            |> Seq.map (Team.getPlaceAfterQuestion gameDay team)
            |> Seq.map (fun p -> p.From.Value)

        teams 
        |> Seq.map places
    
    let vMax = places |> Seq.map (Seq.max) |> Seq.max
    let verticalAxis = {Direction = Direction.Back; Label = "Места"; Maximum = vMax}

    showGraphic places teams gameDay verticalAxis
    
let showPointsQuestionByQuestion gameDay teams = 

    let rightAnswers = 
        let answers team = 
            gameDay
            |> GameDay.allQuestions 
            |> Seq.map (fun q -> Team.totalAnswered gameDay q team)

        teams 
        |> Seq.map answers
    
    let vMax = rightAnswers |> Seq.map (Seq.max) |> Seq.max

    let verticalAxis = {Direction = Forward; Label = "Правильные ответы"; Maximum = Converter.toInt vMax}

    showGraphic rightAnswers teams gameDay verticalAxis
    
let showTotalTable options data = 
        
    let topNResultTable = 
        data
        |> SeasonTable.topNResult options

    let columns = 
            
        let toColumnFormat (table : SeasonRating) = 
            table 
            |> Seq.map (fun (team, points, _) -> team.Name.Value, points)

        // TODO fix
        [topNResultTable; ]
        |> Seq.map toColumnFormat
        
    let optionsString =
        match options.RatingOption, options.FinalDate with
        | FinalGameCounts, _ 
        | FinalGameDoesntCount, NotPlayedYet -> sprintf "Best %d games" options.GamesToCount.Value
        | FinalGameDoesntCount, AlreadyPlayed d -> sprintf "Best %d games (without final %A)" options.GamesToCount.Value d
        
    let gamesAmount = data |> SeasonResults.gamesAmount
        
    let labels = 
        [
            "Team"; 
            optionsString; 
            (sprintf "All %d games" gamesAmount); 
        ]

    GoogleChart.showData columns labels ChartType.Table