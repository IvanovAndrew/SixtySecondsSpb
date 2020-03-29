module SpreadsheetWriter

open Domain
open Domain
open Utils
open SpreadsheetService
open System
open Google

type SheetOptions = 
    {
        FirstQuestion : int
        
        TeamAnswered : string
        Answered : string
        Place : string
        Distance : string
    }

type DataToWrite = 
    {
        TeamAnswers : Answer seq
        RightAnswersOn : int<RightAnswer> seq
        Places : Place seq
        Distance : int<RightAnswer> seq
    }
    
module DataToWrite =
    
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
        

let write sheetOptions spreadsheetId sheetName data = 
    
    async {
        
        let! service = SpreadsheetService.AsyncGetService ServiceMode.ReadWrite 
        let firstRow = sheetOptions.FirstQuestion

        let getRange column valuesCount = 
            sprintf "%s!%s%d:%s%d" sheetName column firstRow column <| firstRow + valuesCount

        let createValueRange column values = 
            let range = getRange column <| Seq.length values
            SpreadsheetService.createValueRange range MajorDimension.Column values

        let update range = 
            
            range 
            |> SpreadsheetService.AsyncUpdateRequest service spreadsheetId
            |> Async.Catch

        let res = 
            [
                // пишем, ответили ли мы
                (sheetOptions.TeamAnswered, data.TeamAnswers |> Seq.map (function Right ->  "'+" | _ -> "") |> Array.ofSeq)
                // пишем число ответивших на вопрос
                (sheetOptions.Answered, data.RightAnswersOn |> Seq.map string |> Array.ofSeq)
                // пишем место
                (sheetOptions.Place, data.Places |> Seq.map (fun p -> sprintf "%d-%d" <| PositiveNum.value p.From <| PositiveNum.value p.To) |> Array.ofSeq)
                // пишем отставание
                (sheetOptions.Distance, data.Distance |> Seq.map string |> Array.ofSeq)
            ]
            |> List.filter(fun (column, _) -> not <| String.IsNullOrEmpty column)
            |> List.map (fun (column, data) -> createValueRange column data)
            |> List.map update
            |> Async.Parallel
            |> Async.RunSynchronously
        
        let totalRes = 
            
            let processStatus acc choice = 
                
                match acc, choice with 
                | Ok(), Choice1Of2 res -> res
                | Ok(), Choice2Of2 (exn : Exception)-> 
                    match exn with 
                    | :? AggregateException as aggrException -> 
                        match aggrException.InnerException with 
                        | :? GoogleApiException as ex -> Error ex.Error.Message
                        | _ -> Error exn.InnerException.Message
                    | _ -> Error <| sprintf "%O" exn
                    
                | Error e, _ -> Error e

            let ok = Ok()
            
            res 
            |> Array.fold processStatus ok

        return totalRes
    }