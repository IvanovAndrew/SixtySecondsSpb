module SixtySeconds.Views

open System.Windows
open Elmish
open Elmish.WPF

open SixtySeconds.Infrastructure
open SixtySeconds.Settings
open SixtySeconds.Domain
open SixtySeconds.Common.CommonTypes
open SixtySeconds.Common.Errors
open SixtySeconds.Common.ErrorMessages
    
[<RequireQualifiedAccess>]
module UrlApp = 
    
    type Model =
        { 
            TableUrl : string
            Day : string
            ErrorMessage : string option
        }
        
    let init() =
        { 
            TableUrl = Config.load TableUrl 
            Day = Config.load Game
            ErrorMessage = None
        }
        
    type CmdMsg =
        | LoadSeasonTable of url : Url
        | LoadGameDay of url : Url * gameName : NoEmptyString
        | StoreValues of (Setting * string) list
        
    type Message =
        | TableUrlEntered of string
        | OnLoadSeasonTableSuccess of SeasonTable
        | OnLoadSeasonTableError of SixtySecondsError
        
        | SeasonTableRequested of Url
        
        | GameDayEntered of string
            
        | GameDayRequested of Url * GameName
        | OnGameDayLoadedSuccess of GameDay
        | OnGameDayLoadedError of SixtySecondsError
        
    let validateGameDay = NoEmptyString.ofString 
    let validateUrl site =
        site
        |> Url.create
        |> Result.bind (function Unknown -> Error "Unexpected site" | x -> Ok x)
    
    let private withError error model =
        {model with ErrorMessage = error |> errorToString |> Some}
        
    let update msg model =
        match msg with
        | TableUrlEntered url -> {model with TableUrl = url; ErrorMessage = None}, []
            
        | GameDayEntered d -> {model with Day = d; ErrorMessage = None}, []
        
        | SeasonTableRequested url -> model, [LoadSeasonTable(url)]

        | OnLoadSeasonTableSuccess seasonTable ->
            
            model, [StoreValues [TableUrl, model.TableUrl]]
            
        | OnLoadSeasonTableError error -> model |> withError error, []
        
        | GameDayRequested (url, gameName) -> model, [LoadGameDay(url, gameName)]
            
        | OnGameDayLoadedSuccess gameDay ->
            
            model, [StoreValues [(TableUrl, model.TableUrl); (Game, gameDay.Name.Value)]]
            
        | OnGameDayLoadedError error -> model |> withError error, []
        
    let bindings() =
            [
            "Url" |> Binding.twoWayValidate(
                (fun m -> m.TableUrl),
                (fun newUrl -> newUrl |> TableUrlEntered),
                (fun model -> model.TableUrl |> validateUrl))

            "Day" |> Binding.twoWayValidate(
                (fun model -> model.Day),
                (fun day -> day |> GameDayEntered),
                (fun m -> m.Day |> validateGameDay))

            "LoadGameDay" |> Binding.cmdIf(
                                fun model -> 
                                    result {
                                        let! url = validateUrl model.TableUrl
                                        let! day = validateGameDay model.Day
    
                                        return GameDayRequested(url, day)
                                    })
        
            "LoadSeasonTable" |> Binding.cmdIf(
                fun model ->
                    model.TableUrl
                    |> Url.create
                    |> Result.map SeasonTableRequested)
            
            "SeasonTableButtonVisibility" |> Binding.oneWay
                                        (fun m -> 
                                            match validateUrl m.TableUrl with
                                            | Ok url -> match url with Sec60Season | Google -> Visibility.Visible | _ -> Visibility.Collapsed
                                            | Error e -> Visibility.Collapsed
                                        )
            "Gameday60SecButtonVisibility" |> Binding.oneWay
                                        (fun m -> 
                                            match validateUrl m.TableUrl with
                                            | Ok url -> match url with Sec60Game _ -> Visibility.Visible | _ -> Visibility.Collapsed
                                            | Error e -> Visibility.Collapsed
                                        )
            
            "GamedayGoogleButtonVisibility" |> Binding.oneWay(fun m -> 
                                            match validateUrl m.TableUrl with
                                            | Ok url -> match url with Google -> Visibility.Visible | _ -> Visibility.Collapsed
                                            | Error e -> Visibility.Collapsed
                                    )
            
            "ErrorMessage" |> Binding.oneWay(fun model -> model.ErrorMessage |> Option.defaultValue "")
        ]
            
    let storeValues values = 
        values
        |> List.iter (fun (setting, value) -> Config.save setting value)
        
    let toCmd cmdMsg =
    
        let ofSuccess success error result =
            match result with
            | Ok value -> success value
            | Error e -> error e
            
        let ofError = Bug
        
        match cmdMsg with 
        | LoadSeasonTable url ->

            let ofSuccess' = ofSuccess OnLoadSeasonTableSuccess OnLoadSeasonTableError
            let ofError' = ofError >> OnLoadSeasonTableError
            
            Cmd.OfAsync.either SixtySecondsApi.parseTotal url ofSuccess' ofError'
            
        | LoadGameDay(url, gameName) ->
            
            let ofSuccess' = ofSuccess OnGameDayLoadedSuccess OnGameDayLoadedError
            let ofError' = ofError >> OnGameDayLoadedError
            
            Cmd.OfAsync.either SixtySecondsApi.parseGameDay (url, gameName) ofSuccess' ofError'
            
        | StoreValues values ->
            
            storeValues values
            Cmd.none