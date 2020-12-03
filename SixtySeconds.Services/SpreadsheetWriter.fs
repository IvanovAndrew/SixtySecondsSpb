namespace SixtySeconds.Services
    
module DataToWrite =
        
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    
    type DataToWrite = 
        {
            TeamAnswers : Answer seq
            RightAnswersOn : int<RightAnswer> seq
            Places : Place seq
            Distance : int<RightAnswer> seq
        }
    
    let fromGameDay gameDay team = 
        let allQuestions = 
            gameDay
            |> GameDay.allQuestions

        {
            TeamAnswers = allQuestions |> Seq.map (fun q -> Team.getAnswer gameDay q team)
            RightAnswersOn = allQuestions |> Seq.map (Question.rightAnswers gameDay)
            Places = allQuestions |> Seq.map (Team.getPlaceAfterQuestion gameDay team)
            Distance = allQuestions |> Seq.map (Team.getGapFromTheFirstPlace gameDay team)
        }

module SpreadsheetWriter = 

    open SixtySeconds.Common.Errors
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    open Shared.Models
    open SpreadsheetService
    open System
    open Google
    open DataToWrite

    let write sheetOptions data : Async<Result<unit, SixtySecondsError>> = 
        
        async {
            
            let! service = SpreadsheetService.AsyncGetService ServiceMode.ReadWrite 
            let firstRow = sheetOptions.ColumnOptions.FirstQuestion

            let getRange column valuesCount = 
                sprintf "%s!%s%d:%s%d" sheetOptions.SheetName column firstRow column <| firstRow + valuesCount

            let createValueRange column values = 
                let range = getRange column <| Seq.length values
                SpreadsheetService.createValueRange range MajorDimension.Column values

            let update range = 
                
                range 
                |> SpreadsheetService.AsyncUpdateRequest service sheetOptions.Id
                |> Async.Catch

            let! res = 
                [
                    // пишем, ответили ли мы
                    (sheetOptions.ColumnOptions.TeamAnswered, data.TeamAnswers |> Seq.map (function Right ->  "'+" | _ -> "") |> Array.ofSeq)
                    // пишем число ответивших на вопрос
                    (sheetOptions.ColumnOptions.Answered, data.RightAnswersOn |> Seq.map string |> Array.ofSeq)
                    // пишем место
                    (sheetOptions.ColumnOptions.Place, data.Places |> Seq.map Place.toString |> Array.ofSeq)
                    // пишем отставание
                    (sheetOptions.ColumnOptions.Distance, data.Distance |> Seq.map string |> Array.ofSeq)
                ]
                |> List.filter(fun (column, _) -> not <| String.IsNullOrEmpty column)
                |> List.map (fun (column, data) -> createValueRange column data)
                |> List.map update
                |> Async.Parallel
            
            let totalRes = 
                
                let processStatus acc choice = 
                    
                    match acc, choice with 
                    | Ok(), Choice1Of2 res -> res |> Result.mapError Bug
                    | Ok(), Choice2Of2 (exn : Exception)-> 
                        match exn with 
                        | :? AggregateException as aggrException -> 
                            match aggrException.InnerException with 
                            | :? GoogleApiException as ex -> Error <| Bug (ex :> exn)
                            | _ -> Error <| Bug exn.InnerException
                        | _ -> Error <| Bug exn
                        
                    | Error _, _ -> acc

                let ok = Ok()
                
                res 
                |> Array.fold processStatus ok

            return totalRes
        }