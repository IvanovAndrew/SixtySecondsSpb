module Chart

open Domain
open Domain
open GoogleChartService
open Utils

let showGraphic data (teams : Team seq) gameDay vAxis = 
    
    let horizontalAxisLabel = "Question"
    
    let graphicData = 
        data 
        |> Seq.map (fun g -> g |> Seq.mapi (fun i v -> i+1, v))

    let labels = teams |> Seq.map (fun t -> t.Name |> NoEmptyString.value)

    let hMax = gameDay.PackageSize |> PositiveNum.value

    let options = 
        {
            HorizonalAxis = {Direction = Direction.Forward; Label = horizontalAxisLabel; Maximum = hMax}
            VerticalAxis = vAxis
            Title = sprintf "%s %s" <| NoEmptyString.value gameDay.Tournament <| NoEmptyString.value gameDay.Name
        }

    GoogleChart.showData graphicData labels <| ChartType.Line options
    
    
let showPlacesQuestionByQuestion gameDay teams = 
    
    let places = 
        let places team = 
            gameDay
            |> GameDay.allQuestions
            |> Seq.map (Team.getPlaceAfterQuestion gameDay team)
            |> Seq.map (fun p -> p.From |> PositiveNum.value)

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
    
let showTotalTable data topN = 
        
    let topNResultTable = 
        data
        |> SeasonTable.topNResult topN

    let columns = 
            
        let toColumnFormat (table : SeasonRating) = 
            table 
            |> Seq.map (fun (team, points, _) -> team.Name |> NoEmptyString.value, points)

        [topNResultTable; data.Table]
        |> Seq.map toColumnFormat
        
    let labels = 
        [
            "Team"; 
            (sprintf "Best %d games" <| PositiveNum.value topN); 
            (sprintf "All %d games" <| PositiveNum.value data.GamesCount); 
        ]

    GoogleChart.showData columns labels ChartType.Table