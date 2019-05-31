module GoogleChartService

type GraphicType = 
    | Places
    | RightAnswers

module GoogleChart = 
    
    open XPlot.GoogleCharts
    open Utils
    open Domain
    open System

    
    type private GoogleChart = XPlot.GoogleCharts.Chart
    type private GoogleAxis = XPlot.GoogleCharts.Configuration.Axis

    type private Direction = 
        | Forward 
        | Back

    type private Axis = 
        {
            Direction : Direction
            Ticks : int array
            Title : string
        }

    let private createAxis a = 
        
        let axis = new GoogleAxis()
        axis.direction <- match a.Direction with | Forward -> 1 | Back -> -1
        axis.ticks <- a.Ticks |> Array.map (fun t -> t :> obj)
        axis.format <- "decimal"
        axis.minValue <- a.Ticks |> Array.head
        axis.title <- a.Title
        axis
        
        //let graphicData team = 
        //    team 
        //    |> places
        //    |> List.mapi (fun i p -> i+1, p)


    let private getData gameDay teams chart = 
        
        match chart with 
        | Places -> 
            
            let places team = 
                gameDay.QuestionsCount 
                |> PositiveNum.createNaturalRange
                |> List.map (GameDay.getPlaceAfterQuestion gameDay team)
                |> List.map (fun p -> p.From |> PositiveNum.value)

            teams 
            |> Seq.map places
        
        | RightAnswers -> 
            
            let answers team = 
                gameDay.QuestionsCount
                |> PositiveNum.createNaturalRange 
                |> List.map (fun q -> GameDay.totalAnswered gameDay team q)

            teams 
            |> Seq.map answers

    let private getOptions (day : DateTime) questionsCount chart = 
        
        let vAxis, hAxis = 
            match chart with 
            | Places -> 
                {Direction = Back; Ticks = [|1..2..21|]; Title = "Место"} 
                |> createAxis,
                {Direction = Forward; Ticks = [|0..4..questionsCount|]; Title = "Номер вопроса"; }
                |> createAxis

            | RightAnswers -> 
                {Direction = Forward; Ticks = [|0..5..30|]; Title = "Правильных ответов"}
                |> createAxis,
                {Direction = Forward; Ticks = [|0..4..questionsCount|]; Title = "Номер вопроса"}
                |> createAxis
        

        XPlot.GoogleCharts.Configuration.Options
            ( 
                title = day.ToShortDateString()
                , curveType = "function"
                , legend = Legend(position = "bottom")
                , vAxis = vAxis
                , hAxis = hAxis
                , height = 800
            )


    let showGraphic gameDay teams chart = 
        
        let toGraphicData data = 
            
            data 
            |> Seq.map (fun g -> g |> List.mapi (fun i v -> i+1, v))

        let data = 
            chart 
            |> getData gameDay teams 
            |> toGraphicData

        let options = 
            let questionsCount = gameDay.QuestionsCount  |> PositiveNum.value
            getOptions gameDay.Day questionsCount chart 

        let labels = teams |> Seq.map (fun t -> t.Name) |> Seq.map NoEmptyString.value

        data
        |> GoogleChart.Line
        |> GoogleChart.WithOptions options
        |> GoogleChart.WithLabels labels
        |> GoogleChart.Show