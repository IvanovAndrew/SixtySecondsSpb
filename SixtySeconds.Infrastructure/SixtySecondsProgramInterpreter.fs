namespace SixtySeconds.Infrastructure

open SixtySeconds.Data

module SixtySecondsProgramInterpreter =
    
    open SixtySeconds.Common.Errors
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.SixtySecondsProgramBuilder
    
    let private parseGameDay (url, game) =
        SixtySecondsDataPipeline.parseGameDayAsync url game
        
    let private gameDayRating = SixtySecondsDataPipeline.getGameDayRating
        
    let private parseSeasonRating = SixtySecondsDataPipeline.parseSeasonRating
    let private topNResultsTable (seasonTable, games) = SixtySecondsDataPipeline.topNResultsTable seasonTable games
    
    let rec private interpretSixtySecondsProgram (prog : Program<'a>) =
        match prog with
        | ParseGameDay ((url,game), next) ->
            (url,game) |> parseGameDay |> bindAsync (next >> interpretSixtySecondsProgram)
            
        | GameDayRating (gameDay, next) ->
            gameDay |> gameDayRating |> bindAsync (next >> interpretSixtySecondsProgram)
                
        | ParseSeasonRating (url, next) ->
            url |> parseSeasonRating |> bindAsync (next >> interpretSixtySecondsProgram)
            
        | TopNResultsTable (seasonTable, next) ->
            seasonTable |> topNResultsTable |> bindAsync (next >> interpretSixtySecondsProgram)
        
        | Stop a -> async.Return a
        | _ -> failwithf "Not implemented"
        
    let interpret (prog : Program<'a>) =
        try
            let interpret' = interpretSixtySecondsProgram
            interpret' prog
        with
        | failure -> failure |> bug |> async.Return

    let interpretSimple prog =
        try
            async {
                let! result = interpretSixtySecondsProgram prog
                return Ok result
            }
        with
        | failure -> failure |> bug |> async.Return