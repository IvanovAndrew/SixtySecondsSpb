module SixtySeconds.Views

open System.Windows
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
        
    type Message =
        | TableUrlEntered of string
        | OnLoadSeasonTableSuccess of SeasonTable
        | OnLoadSeasonTableError of SixtySecondsError
        
        | SeasonTableRequested of Url
        
        | GameDayEntered of string
            
        | GameDayRequested of Url * NoEmptyString
        | OnGameDayLoadedSuccess of GameDay
        | OnGameDayLoadedError of SixtySecondsError

        | StoreValues of (Setting * string) list
        
    let validateGameDay = NoEmptyString.ofString 
    let validateUrl = Url.create
        
    let update msg model =
        match msg with
        | TableUrlEntered url -> {model with TableUrl = url; ErrorMessage = None}, []
            
        | GameDayEntered d -> {model with Day = d; ErrorMessage = None}, []
        
        | SeasonTableRequested url -> model, [LoadSeasonTable(url)]

        | OnLoadSeasonTableSuccess seasonTable ->
            StoreValues [TableUrl, model.TableUrl] |> ignore
            // TODO 
            model, []
            
            
        | OnLoadSeasonTableError error ->
            {model with ErrorMessage = error |> errorToString |> Some }, []
        
        | GameDayRequested (url, gameName) -> model, [LoadGameDay(url, gameName)]
            
        | OnGameDayLoadedSuccess gameDay ->
            
            ignore <| StoreValues [(TableUrl, model.TableUrl); (Game, gameDay.Name.Value)]
            // TODO 
            model, []
            
        | OnGameDayLoadedError error ->
            {model with ErrorMessage = error |> errorToString |> Some}, []
                
        | StoreValues values ->
            
            values
            |> List.iter (fun (setting, value) -> Config.save setting value)
            
            model, []
        
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
                                            | Ok url -> match url with Sec60Game -> Visibility.Visible | _ -> Visibility.Collapsed
                                            | Error e -> Visibility.Collapsed
                                        )
            
            "GamedayGoogleButtonVisibility" |> Binding.oneWay(fun m -> 
                                            match validateUrl m.TableUrl with
                                            | Ok url -> match url with Google -> Visibility.Visible | _ -> Visibility.Collapsed
                                            | Error e -> Visibility.Collapsed
                                    )
            
            "ErrorMessage" |> Binding.oneWay(fun model -> model.ErrorMessage |> Option.defaultValue "")
        ]
        
    let loadGameDay (url, game) =
        async {
            let! document = url |> Parser.asyncLoadDocument 
            
            return
                document
                |> expectWebRequestError
                |> Result.bind (Parser.parse game >> expectParsingError)
        }
    
    let toCmd cmdMsg (wrap : Message -> 'a) toSeasonPage toGamedayPage =
    
        let ofSuccess result success error =
            match result with
            | Ok value -> success value
            | Error e -> error e
            
        let ofError = Bug
        
        match cmdMsg with 
        | LoadSeasonTable url ->
            let ofSuccess' result =
                ofSuccess result toSeasonPage (OnLoadSeasonTableError >> wrap)
                
            let ofError' = ofError >> (OnLoadSeasonTableError >> wrap)
            
            Elmish.Cmd.OfAsync.either SixtySecondsApi.parseTotal url ofSuccess' ofError'
            
        | LoadGameDay(url, gameName) ->
            
            let ofSuccess' result = ofSuccess result toGamedayPage (OnGameDayLoadedError >> wrap)
            let ofError' = ofError >> (OnGameDayLoadedError >> wrap)
            
            Elmish.Cmd.OfAsync.either SixtySecondsApi.parseGameDay (url, gameName) ofSuccess' ofError'