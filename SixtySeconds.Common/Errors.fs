namespace SixtySeconds.Common

module Errors =
    
    type ParsingError =
    | MissingSheetName
    | MissingTournamentName
    | MissingAnswersCount
    | TeamParsingError of string
    | AnswersParsingError of string
    | SheetNotFound of string
    | DuplicatedTeam of Team
    | SeasonHasNotStarted
    
    
    type WebRequestError = 
        | PageNotFound of string
        | UnexpectedResponseBody 
    
    type SixtySecondsError =
        | ParsingError of ParsingError
        | WebRequestError of WebRequestError