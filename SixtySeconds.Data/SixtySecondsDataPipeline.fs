namespace SixtySeconds.Data

open FSharp.Data

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
    
    
    
    
    let private parseTournamentInfo document =
        
        let headerDiv =
            document
            |> HtmlDocument.body
            |> HtmlNode.elementWithId "header"
        
        let grandChild = 
            headerDiv
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
        
        let gameName = 
            grandChild
            |> HtmlNode.firstElement
            |> HtmlNode.innerText
        
        
        let city, league, seasonName =
            let lastChild = 
                grandChild
                |> HtmlNode.elements
                |> List.last
                |> HtmlNode.elements
            
            match lastChild with
            | [city; league; _; season; _ ] ->
                city |> HtmlNode.innerText,
                league |> HtmlNode.innerText,
                season |> HtmlNode.innerText
                
            | _ -> "", "", ""
        
        result {
            let! cityName = city |> NoEmptyString.ofString |> expectMissingCityName
            let! leagueName = league |> NoEmptyString.ofString |> expectMissingLeagueName
            let! seasonName = seasonName |> NoEmptyString.ofString |> expectMissingSeasonName
            
            return
                {
                    City = cityName
                    League = leagueName
                    Season = seasonName
                }
        }
        
    let private parseGamename document =
        
        let headerDiv =
            document
            |> HtmlDocument.body
            |> HtmlNode.elementWithId "header"
        
        let grandChild = 
            headerDiv
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
        
        grandChild
        |> HtmlNode.firstElement
        |> HtmlNode.innerText
        |> NoEmptyString.ofString
        |> expectMissingGameName
        
        
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
                    let! gameName = parseGamename document |> expectParsingError
                    
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
                |> Result.bind (fun d -> d |> Parser.parseGameday game |> expectParsingError)
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