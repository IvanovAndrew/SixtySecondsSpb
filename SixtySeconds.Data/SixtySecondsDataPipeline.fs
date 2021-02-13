namespace SixtySeconds.Data

open FSharp.Data
open SixtySeconds.Data.DataLoader

module SixtySecondsDataPipeline =
    
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.Common.Errors
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    
    open Parser
    
    type ParseGameDayAsync = Url -> GameName -> AsyncResult<GameDay, SixtySecondsError>
    type GameDayRatingAsync = GameDay -> Async<GameDayRating>
    type ParseSeasonRating = Url -> AsyncResult<SixtySecondsSeason * MatrixSeason, SixtySecondsError>
    type TopNResultsTable = SeasonResultFilter -> SeasonResults -> Async<SeasonRating>        
        
    let private asyncParse60SecondSite gameId =
        async {
        
            let! tournamentDocument =
                gameId
                |> sprintf "https://60sec.online/game/%d"
                |> Url.create
                |> Result.valueOrException
                |> asyncLoadDocument
                
            let sec60url = 
                gameId
                |> sprintf "https://60sec.online/get_game_results/?game_id=%d&key=1000"
                |> Url.create
                |> Result.valueOrException
                
            let parse json =
                result {
                    let! document = tournamentDocument |> expectWebRequestError
                    let! tournament = parseTournamentInfo document |> expectParsingError
                    let! gameName = parseGameName document |> expectParsingError
                    
                    return! parse60SecondGameDay tournament gameName json |> expectParsingError
                }
        
            let! doc = sec60url |> Url.value |> JsonValue.AsyncLoad 
            return parse doc
        }
        
    let private asyncParseGoogleSpreadsheet game url =
        
        async {
            let! document = url |> asyncLoadDocument 
            
            return
                document
                |> expectWebRequestError
                |> Result.bind (fun d -> d |> parseGameday game |> expectParsingError)
        }
    
    let parseGameDayAsync : ParseGameDayAsync =
        fun url game ->
            async {
                return!
                    match url with
                    | Sec60Game gameId -> asyncParse60SecondSite gameId
                    | Google -> asyncParseGoogleSpreadsheet game url
                    | _ -> async {return url |> Url.value |> UnexpectedSite |> ParsingError |> Error}
            }
            
    
        
    let getGameDayRating : GameDayRatingAsync =
        fun gameDay -> async {return gameDay |> Rating.ofGameDay }
            
    let parseSeasonRating : ParseSeasonRating =
        fun url ->
            async {
                let! htmlDocument = url |> asyncLoadDocument
                
                let parse =
                    match url with
                    | Sec60Season -> parseTotalFrom60SecSite
                    //| Google -> parseTotalFromGoogleSpreadsheet
                    | _ -> (fun _ -> url |> Url.value |> UnexpectedSite |> Error)
                
                return
                    htmlDocument
                    |> expectWebRequestError
                    |> Result.bind (fun doc -> parse doc |> expectParsingError)
            }
            
    let topNResultsTable : TopNResultsTable =
        fun options seasonTable  ->
            async {
                    return seasonTable |> SeasonTable.topNResult options 
            }