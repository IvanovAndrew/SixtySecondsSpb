module Shared.Api

open Shared.Models

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ISixtySecondsApi =
    {
        parseGameDay : string -> Async<Result<GameDayModel, string>>
        gameDayRating : GamedayRatingTypeModel * GameDayModel -> Async<Result<(TeamModel * decimal * PlaceModel) list, string>>
        teamPerformance : GameDayModel * TeamModel -> Async<Result<TeamPerformanceModel, string>>
        parseSeasonRating : string -> Async<Result<SixtySecondsSeasonModel * MatrixSeasonModel, string>>
        filterRating : RatingFilterModel * SeasonResultModel -> Async<Result<(TeamModel * decimal * PlaceModel) list, string>>
        writeToSpreadsheet: (SpreadsheetOptions * GameDayModel * TeamModel) -> Async<Result<unit, string>>
        showChart: (ChartType * GameDayModel) -> Async<unit>
    }