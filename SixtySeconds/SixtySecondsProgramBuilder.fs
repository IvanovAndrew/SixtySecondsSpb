namespace SixtySeconds

module SixtySecondsProgramBuilder =
    
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.Common.Errors
    open SixtySeconds.Domain
    
    type Program<'a> =
        | ParseGameDay of (Url * GameName) * (Result<GameDay, SixtySecondsError> -> Program<'a>)
        | GameDayRating of gameDay : GameDay * (GameDayRating -> Program<'a>)
        
        | ParseSeasonRating of url : Url * (Result<SeasonTable, SixtySecondsError> -> Program<'a>)
        | TopNResultsTable of (SeasonTable * PositiveNum) * (SeasonRating -> Program<'a>)
        | Stop of 'a
        
    // This bind function allows you to pass a continuation for current node of your expression tree
    // the code is basically a boiler plate, as you can see.
    let rec bind f instruction =
        match instruction with
        | ParseGameDay (x, next) -> ParseGameDay (x, (next >> bind f))
        | GameDayRating (x, next) -> GameDayRating (x, (next >> bind f))
        | ParseSeasonRating (x, next) -> ParseSeasonRating (x, (next >> bind f))
        | TopNResultsTable (x, next) -> TopNResultsTable (x, next >> bind f)
        | Stop x -> f x
            
            
    let stop x = Stop x
    let parseGameDay url gameName = ParseGameDay ((url, gameName), stop)
    let gameDayRating gameDay = GameDayRating(gameDay, stop)
    let parseSeasonRating url = ParseSeasonRating (url, stop)
    let topNResult seasonTable games = TopNResultsTable((seasonTable, games), stop)
            
    // These are builders for computation expressions. Using CEs will make building execution trees very easy
    type SimpleProgramBuilder() =
        member __.Bind (x, f) = bind f x
        member __.Return x = Stop x
        member __.Zero () = Stop ()
        member __.ReturnFrom x = x

    type ProgramBuilder() =
        member __.Bind (x, f) = bind f x
        member this.Bind (x, f) =
            match x with
            | Ok x -> this.ReturnFrom (f x)
            | Error e -> this.Return (Error e)
        member this.Bind((x: Program<Result<_,_>>), f) =
            let f x =
                match x with
                | Ok x -> this.ReturnFrom (f x)
                | Error e -> this.Return (Error e )
            this.Bind(x, f)
        member __.Return x = Stop x
        member __.Zero () = Stop ()
        member __.ReturnFrom x = x

    let program = ProgramBuilder()
    let simpleProgram = SimpleProgramBuilder()