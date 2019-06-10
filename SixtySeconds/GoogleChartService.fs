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

    [<RequireQualifiedAccess>]
    type private ChartOption = 
        | Places of int
        | RightAnswers of int

    let private createAxis a = 
        
        let axis = new GoogleAxis()
        axis.direction <- match a.Direction with | Forward -> 1 | Back -> -1
        axis.ticks <- a.Ticks |> Array.map (fun t -> t :> obj)
        axis.format <- "decimal"
        axis.minValue <- a.Ticks |> Array.head
        axis.title <- a.Title
        axis
        

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
        
        let hAxis = 
            {Direction = Forward; Ticks = [|0..2..questionsCount|]; Title = "Номер вопроса"; }
            |> createAxis

        let vAxis = 
            match chart with 
            | ChartOption.Places worstPlace -> 
                {Direction = Back; Ticks = [|1..2..worstPlace|]; Title = "Место"} 
                |> createAxis

            | ChartOption.RightAnswers maxRightAnswers -> 
                {Direction = Forward; Ticks = [|0..3..maxRightAnswers|]; Title = "Правильных ответов"}
                |> createAxis
                
        

        Configuration.Options
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

            let chartOptions = 
                match chart with 
                | Places -> 
                    
                    let worstPlace = 
                        data 
                        |> Seq.map (fun l -> l |>  List.map snd |> List.max)
                        |> Seq.max

                    ChartOption.Places worstPlace
                | RightAnswers -> 
                    
                    let maxRightAnswers = 
                        data 
                        |> Seq.map (fun l -> l |>  List.map snd |> List.max)
                        |> Seq.max
                    
                    ChartOption.RightAnswers maxRightAnswers

            getOptions gameDay.Day questionsCount chartOptions

        let labels = teams |> Seq.map (fun t -> t.Name) |> Seq.map NoEmptyString.value

        data
        |> GoogleChart.Line
        |> GoogleChart.WithOptions options
        |> GoogleChart.WithLabels labels
        |> GoogleChart.Show