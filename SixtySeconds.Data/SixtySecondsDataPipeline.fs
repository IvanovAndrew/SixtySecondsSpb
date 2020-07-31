namespace SixtySeconds.Data

module SixtySecondsDataPipeline =
    
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.Common.Errors
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    
    open Parser
    
    type ParseGameDayAsync = Url -> GameName -> AsyncResult<GameDay, SixtySecondsError>
    type GameDayRatingAsync = GameDay -> Async<GameDayRating>
    type ParseSeasonRating = Url -> AsyncResult<SeasonTable, SixtySecondsError>
    type TopNResultsTable = SeasonTable -> PositiveNum -> Async<SeasonRating>
    
    let parseGameDayAsync : ParseGameDayAsync =
        fun url game ->
            async {
                let! htmlDocument = url |> asyncLoadDocument 
                return
                    htmlDocument
                    |> expectWebRequestError
                    |> Result.bind (fun d -> d |> Parser.parse game |> expectParsingError)
            }
        
    let getGameDayRating : GameDayRatingAsync =
        fun gameDay -> async {return gameDay |> Rating.ofGameDay }
            
    let parseSeasonRating : ParseSeasonRating =
        fun url ->
            async {
                let! htmlDocument = url |> asyncLoadDocument
                
                let parse =
                    match url with
                    | Sec60Season -> Parser.parseTotalFrom60SecSite
                    | Google -> Parser.parseTotalFromGoogleSpreadsheet
                    | _ -> (fun _ -> url |> Url.value |> UnexpectedSite |> Error)
                
                return
                    htmlDocument
                    |> expectWebRequestError
                    |> Result.bind (fun doc -> parse doc |> expectParsingError)
            }
            
    let topNResultsTable : TopNResultsTable =
        fun seasonTable gamesToCount ->
            async {
                    return seasonTable |> SeasonTable.topNResult gamesToCount
            }