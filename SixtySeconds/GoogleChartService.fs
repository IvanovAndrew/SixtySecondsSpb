module GoogleChartService
    
open XPlot.GoogleCharts
open Utils
open OutputParams
    

let showGraphic graphicData = 
    
    let showChart chart = 
    
        let getVAxis() = 
            
            let vTicks = 
                [1..3..21]
                |> List.map ( fun n -> n :> obj)
                |> Array.ofList

            let axis = new Axis()
            axis.direction <- -1
            axis.ticks <- vTicks

            axis

        let getHAxis() = 
            
            let hTicks = 
                [1..2..36]
                |> List.map ( fun n -> n :> obj)
                |> Array.ofList

            let axis = new Axis()
            axis.format <- "decimal"
            axis.ticks <- hTicks
            axis.minValue <- 0
            axis
        

        let chartOptions = new Options()
        chartOptions.vAxis <- getVAxis()
        chartOptions.hAxis <- getHAxis()
        
        chart 
        |> Chart.WithOptions chartOptions
        |> Chart.WithHeight 600
        |> Chart.WithTitle (graphicData.Team.Name |> NoEmptyString.value)
        |> Chart.WithXTitle "Номер вопроса"
        |> Chart.WithYTitle "Место"
        |> Chart.Show

    graphicData.Places
    |> Seq.map ((fun p -> p.From) >> PositiveNum.value)
    |> Seq.mapi (fun i place -> (i+1), place)
    |> Chart.Line
    |> showChart