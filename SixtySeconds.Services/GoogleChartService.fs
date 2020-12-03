namespace SixtySeconds.Services

module GoogleChartService = 

    type Direction = 
        | Forward
        | Back

    type AxisOption = 
        {
            Direction : Direction
            Label : string
            Maximum : int
        }

    type ChartOptions = 
        {
            HorizonalAxis : AxisOption
            VerticalAxis : AxisOption
            Title : string
        }
        
    [<RequireQualifiedAccess>]
    type ChartType = 
        | Table 
        | Line of ChartOptions

module GoogleChart = 
    
    open XPlot.GoogleCharts
    open System
    open GoogleChartService

    
    type private GoogleChart = XPlot.GoogleCharts.Chart
    type private GoogleAxis = XPlot.GoogleCharts.Configuration.Axis

    type private Axis = 
        {
            Direction : Direction
            Ticks : int array
            Title : string
        }

    let private createAxis a = 
        
        let axis = new GoogleAxis()
        axis.direction <- match a.Direction with | Forward -> 1 | Back -> -1
        axis.ticks <- a.Ticks |> Array.map  (fun t -> t :> obj)
        axis.format <- "decimal"
        axis.minValue <- a.Ticks |> Array.head
        axis.title <- a.Title
        axis.maxTextLines <- 1
        axis
        

    let private getOptions (graphicParams : ChartOptions) = 
        
        let hAxis = 
            
            let axisOption = graphicParams.HorizonalAxis

            {
                Direction = axisOption.Direction
                Ticks = [|0..2..axisOption.Maximum|]; 
                Title = axisOption.Label 
            }
            |> createAxis

        let vAxis = 
            
            let axisOption = graphicParams.VerticalAxis
            {
                Direction = axisOption.Direction
                Ticks = [|1..2..axisOption.Maximum|]; 
                Title = axisOption.Label
            } 
            |> createAxis
                
        let legend = 
            Legend
                (
                    alignment = "center"
                    , position = "top"
                    , textStyle = TextStyle(fontSize = 16)
                )
        

        Configuration.Options
            ( 
                title = graphicParams.Title
                , curveType = "function"
                , legend = legend
                , vAxis = vAxis
                , hAxis = hAxis
                , height = 800
            )
            

    let showData (data : seq<seq<_>>) labels chartType = 
        
        async {
        
            let options = 
                match chartType with 
                | ChartType.Table -> Options(showRowNumber = true)
                | ChartType.Line options -> 
                    getOptions options

            let chart = 
                match chartType with 
                | ChartType.Table -> 
                    data
                    |> GoogleChart.Table
                | ChartType.Line _ -> data |> GoogleChart.Line

            chart
            |> GoogleChart.WithOptions options
            |> GoogleChart.WithLabels labels
            |> GoogleChart.Show
        }