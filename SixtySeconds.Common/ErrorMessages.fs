module SixtySeconds.Common.ErrorMessages

open Errors

let errorToString = function
    | ParsingError error ->
        match error with 
        | MissingSheetName -> "Missing sheet name"
        | MissingTournamentName -> "Missing tournament name"
        | MissingAnswersCount -> "Missing answers count"
        | AnswersParsingError err -> sprintf "Can not answers. %s" err
        | TeamParsingError err -> sprintf "Can not parse team. %s" err
        | SheetNotFound sheetName -> sprintf "Sheet %s not found" sheetName
        | SeasonHasNotStarted -> "Season hasn't started yet"
        | DuplicatedTeam team -> sprintf "Team %s is already added " team
        | UnexpectedSite url -> sprintf "Wrong site type %s" url
    | WebRequestError error ->
        match error with
        | PageNotFound url -> sprintf "Page %s not found" url
        | UnexpectedResponseBody -> "Unexpected response body"
    | Bug exc -> sprintf "Unhandled exception: %s" <| exc.Message