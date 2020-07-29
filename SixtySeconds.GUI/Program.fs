module SixtySeconds.GUI

open System
open System.Windows

open Elmish
open Elmish.WPF

open SixtySecond.GUI.Settings
open SixtySeconds.Views
open SixtySeconds.Common.CommonTypes

open Domain

let validateGameDay = NoEmptyString.ofString 

let validateUrl = Url.create

type Model =
    { 
        TableUrl : string
        Day : string
        ErrorMessage : string option

        SeasonTableModel : SeasonTableApp.Model option
        GameDayModel : GameDayApp.Model option
    }

let init() =

    { 
        TableUrl = Config.load TableUrl 
        Day = Config.load Game
        ErrorMessage = None

        SeasonTableModel = None
        GameDayModel = None
    }, Cmd.none
    
let loadSeasonTable url =
    async {
        let! document = url |> Parser.asyncLoadDocument 

        return 
            document
            |> Result.mapError WebRequestError
            |> Result.bind (Parser.parseTotal >> Result.mapError ParsingError)
    }
    
let loadGameDay (url, game) =
    async {
        let! document = url |> Parser.asyncLoadDocument 
        
        return
            document
            |> Result.mapError WebRequestError
            |> Result.bind (Parser.parse game >> Result.mapError ParsingError)
    }

type Message =
    | TableUrlEntered of string
    | LoadSeasonTable of Url
    | OnSeasonTableLoaded of Result<SeasonTable, SixtySecondsError>
    | OnLoadSeasonTableSuccess of SeasonTable
    | OnLoadSeasonTableError of SixtySecondsError
    | SeasonTableMessage of SeasonTableApp.Message
    | SeasonTableCloseRequested
    
    
    | GameDayEntered of string
    
    | LoadGameDay of Url * NoEmptyString
    | OnGameDayLoaded of Result<GameDay, SixtySecondsError>
    | OnGameDayLoadedSuccess of GameDay
    | OnGameDayLoadedError of SixtySecondsError
    | GameDayCloseRequested

    | GameDayMessage of GameDayApp.Message

    | StoreValues of (Setting * string) list
    
    
let resultToMessage successMessage errorMessage result =
    
    match result with
    | Ok result -> successMessage result
    | Error error -> errorMessage error

// https://medium.com/@MangelMaxime/my-tips-for-working-with-elmish-ab8d193d52fd

let update msg model =
    match msg with
    | TableUrlEntered url -> {model with TableUrl = url; ErrorMessage = None}, Cmd.none
        
    | GameDayEntered d -> {model with Day = d; ErrorMessage = None}, Cmd.none
    
    | LoadSeasonTable url -> model, Cmd.OfAsync.perform loadSeasonTable url OnSeasonTableLoaded
    | OnSeasonTableLoaded result ->
        
        let message =
            result
            |> resultToMessage OnLoadSeasonTableSuccess OnLoadSeasonTableError
        model, Cmd.ofMsg <| message
        
    
    | OnLoadSeasonTableSuccess seasonTable ->
        {
            model with SeasonTableModel = Some <| SeasonTableApp.initModel seasonTable
        },
        Cmd.ofMsg <| StoreValues [TableUrl, model.TableUrl]
        
    | OnLoadSeasonTableError error ->
        {model with ErrorMessage = error |> errorToString |> Some }, Cmd.none
    
    | LoadGameDay(url, gameName) ->
        model, Cmd.OfAsync.perform loadGameDay (url, gameName) OnGameDayLoaded 
        
    | OnGameDayLoaded result ->
        let nextCommand =
            result
            |> resultToMessage OnGameDayLoadedSuccess OnGameDayLoadedError
            |> Cmd.ofMsg
        model, nextCommand
        
    | OnGameDayLoadedSuccess gameDay ->
        
        {model with GameDayModel = GameDayApp.init gameDay |> Some},
        Cmd.ofMsg <| StoreValues [(TableUrl, model.TableUrl); (Game, gameDay.Name |> NoEmptyString.value)] 
    
    | OnGameDayLoadedError error ->
        {model with ErrorMessage = error |> errorToString |> Some}, Cmd.none
            
    | GameDayMessage message -> 
        {
            model with 
                GameDayModel = 
                    model.GameDayModel |> Option.map (GameDayApp.update message)
        }, Cmd.none
    | GameDayCloseRequested -> {model with GameDayModel = None}, Cmd.none
    
    | SeasonTableMessage message -> 
        {
            model with 
                SeasonTableModel = 
                    model.SeasonTableModel |> Option.map(SeasonTableApp.update message)
        }, Cmd.none
    | StoreValues values ->
        
        values
        |> List.iter (fun (setting, value) -> Config.save setting value)
        
        model, Cmd.none
    | SeasonTableCloseRequested -> {model with SeasonTableModel = None}, Cmd.none
    

let bindings () : Binding<Model, Message> list = [
    "Url" |> Binding.twoWayValidate(
        (fun m -> m.TableUrl),
        (fun newUrl -> TableUrlEntered newUrl),
        (fun model -> model.TableUrl |> validateUrl))

    "Day" |> Binding.twoWayValidate(
        (fun model -> model.Day),
        (fun day -> GameDayEntered day),
        (fun m -> m.Day |> validateGameDay))

    "LoadGameDay" |> Binding.cmdIf(
                                fun model -> 
                                    result {
                                        let! url = validateUrl model.TableUrl
                                        let! day = validateGameDay model.Day

                                        return LoadGameDay(url, day)
                                    })
    "LoadSeasonTable" |> Binding.cmdIf(
        fun model -> model.TableUrl |> Url.create |> Result.map LoadSeasonTable)

    "SeasonTableWin" |> Binding.subModelWin(
        (fun m -> m.SeasonTableModel |> WindowState.ofOption), 
        snd, 
        id,
        SeasonTableApp.bindings SeasonTableMessage,
        (fun () -> SixtySeconds.Views.SeasonTableWindow(Owner = Application.Current.MainWindow)),
        onCloseRequested = SeasonTableCloseRequested,
        isModal = true
      )

    "GameDayWindow" |> Binding.subModelWin(
        (fun m -> m.GameDayModel |> WindowState.ofOption), 
        snd, 
        id,
        GameDayApp.bindings GameDayMessage,
        (fun () -> SixtySeconds.Views.GameDayWindow(Owner = Application.Current.MainWindow)),
        onCloseRequested = GameDayCloseRequested,
        isModal = true
    )

    "ErrorMessage" |> Binding.oneWay(fun model -> model.ErrorMessage |> Option.defaultValue "")
]

let private createWindow() = 
    
    let window = MainWindow()
    //window.Closed.AddHandler((fun _ _ -> Settings.Default.Save()))

    window

[<EntryPoint; STAThread>]
let main _ =

    let mainWindow = createWindow()
    
    Program.mkProgramWpf init update bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true; Measure = true }
        (mainWindow)