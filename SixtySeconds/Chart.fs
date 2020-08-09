﻿module Chart

open SixtySeconds.Domain
open SixtySeconds.Actions
open GoogleChartService
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
    
let showTotalTable data topN = 
        
    let topNResultTable = 
        data
        |> SeasonTable.topNResult topN

    let columns = 
            
        let toColumnFormat (table : SeasonRating) = 
            table 
            |> Seq.map (fun (team, points, _) -> team.Name.Value, points)

        [topNResultTable; data.Table]
        |> Seq.map toColumnFormat
        
    let labels = 
        [
            "Team"; 
            (sprintf "Best %d games" topN.Value); 
            (sprintf "All %d games" data.GamesCount.Value); 
        ]

    GoogleChart.showData columns labels ChartType.Table