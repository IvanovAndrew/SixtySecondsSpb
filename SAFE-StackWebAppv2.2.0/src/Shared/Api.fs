module Shared.Api

open Shared.Models

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ISixtySecondsApi =
    {
        parseGameDay : string -> Async<Result<GameDayModel, string>>
        parseSeasonRating : string -> Async<Result<SeasonTableModel, string>>
        writeToSpreadsheet: (SpreadsheetOptions * GameDayModel * TeamModel) -> Async<Result<unit, string>>
        showChart: (ChartType * GameDayModel) -> Async<unit>
    }