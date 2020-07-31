﻿namespace SixtySeconds.Common

module Errors =
    
    type ParsingError =
    | MissingSheetName
    | MissingTournamentName
    | MissingAnswersCount
    | TeamParsingError of string
    | AnswersParsingError of string
    | SheetNotFound of string
    | DuplicatedTeam of string
    | SeasonHasNotStarted
    | UnexpectedSite of string
    
    
    type WebRequestError = 
        | PageNotFound of string
        | UnexpectedResponseBody 
    
    type SixtySecondsError =
        | ParsingError of ParsingError
        | WebRequestError of WebRequestError
        | Bug of exn
        
    let bug exc = exc |> Bug |> Error
        
    let pageNotFound url = url |> PageNotFound |> Error
    let sheetNotFound sheetName = sheetName |> SheetNotFound |> Error
    let unexpectedResponse()  = UnexpectedResponseBody |> Error
    
    let expectWebRequestError result = result |> Result.mapError WebRequestError
    let expectParsingError result = result |> Result.mapError ParsingError
    let expectTeamParsingError result = result |> Result.mapError TeamParsingError
    let expectDuplicatedTeam result = result |> Result.mapError DuplicatedTeam
    let expectMissingTournamentName result = result |> Result.mapError (fun _ -> MissingTournamentName)
    let expectSeasonHasNotStartedError result = result |> Result.mapError (fun _ -> SeasonHasNotStarted)
    let expectMissingAnswers result = result |> Result.mapError (fun _ -> MissingAnswersCount)
    
    
    (*
    Some type aliases for making code more readable and for preventing
    typo-kind of mistakes: so you don't declare a validation function with
    plain `Error` type, for example.
    *)
    type AsyncResult<'a, 'error> = Async<Result<'a, 'error>>
    type IoResult<'a> = AsyncResult<'a, SixtySecondsError>
    type PipelineResult<'a> = AsyncResult<'a, SixtySecondsError>
    type IoQueryResult<'a> = Async<'a option>