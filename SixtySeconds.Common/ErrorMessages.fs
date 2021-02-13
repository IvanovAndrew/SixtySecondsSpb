module SixtySeconds.Common.ErrorMessages

open Errors

let errorToString = function
    | ParsingError error ->
        match error with 
        | MissingSheetName -> "Missing sheet name"
        | MissingCityName -> "Missing city name"
        | MissingLeagueName -> "Missing league name"
        | MissingSeasonName -> "Missing season name"
        | MissingGameName -> "Missing game name"
        | MissingAnswersCount -> "Missing answers count"
        | CantParseDate d -> sprintf "Can't parse string %s as date" d
        | AnswersParsingError err -> sprintf "Can not parse answers. %s" err
        | TableNotFound tableid -> sprintf "Table with id %s not found" tableid
        | TableColumnNotFound column -> sprintf "Table column %s not found" column
        | TeamParsingError err -> sprintf "Can not parse team. %s" err
        | SheetNotFound sheetName -> sprintf "Sheet %s not found" sheetName
        | SeasonHasNotStarted -> "Season hasn't started yet"
        | BusinessError e ->
            match e with
            | TeamAlreadyAdded team -> sprintf "Team %s is already added " team
            | QuestionsCountMismatching (actual, expected) -> sprintf "Questions mismatching: actual %d, expected %d" actual expected 
        | UnexpectedSite url -> sprintf "Wrong site type %s" url
        | UnexpectedJson str -> sprintf "Unexpected json %s" str
    | WebRequestError error ->
        match error with
        | PageNotFound url -> sprintf "Page %s not found" url
        | UnexpectedResponseBody -> "Unexpected response body"
    | Bug exc -> sprintf "Unhandled exception: %s" <| exc.Message
