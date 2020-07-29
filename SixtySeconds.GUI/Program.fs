module SixtySeconds.GUI

open System
open System.Windows

open Elmish
open Elmish.WPF

open GameDayApp
open MainPageApp
open SixtySeconds.Views
open SixtySeconds.Common.CommonTypes

open SixtySeconds.Domain

type AppWindow =
    | Main
    | SeasonTable
    | GameDay
    

type Model =
    {
        FirstPageState : MainApp.Model
        SeasonTableState : SeasonTableApp.Model option
        GameDayState : GameDayApp.Model option
        
        Window : AppWindow
    }
    
type CmdMsg =
    | MainPageCmd of MainApp.CmdMsg
    | GameDayPageCmd of GameDayApp.CmdMsg

let init() =

    { 
        FirstPageState = MainApp.init() 
        SeasonTableState = None
        GameDayState = None
        
        Window = Main
    }, []
    
type NewPage =
    | ToMainPage
    | ToSeasonTablePage of SeasonTable
    | ToGameDayPage of GameDay

type Message =
    | MainPageMessage of MainApp.Message
    | GameDayPageMessage of GameDayApp.Message
    | SeasonTableMessage of SeasonTableApp.Message
    
    | SwitchPage of NewPage
    
    
let resultToMessage successMessage errorMessage result =
    
    match result with
    | Ok result -> successMessage result
    | Error error -> errorMessage error

// https://medium.com/@MangelMaxime/my-tips-for-working-with-elmish-ab8d193d52fd

let update msg (model : Model) =
    match model.Window, msg with
    | Main, MainPageMessage msg ->
        let nextState, s = MainApp.update msg model.FirstPageState
        {model with FirstPageState = nextState}, s |> List.map MainPageCmd
        
    | SeasonTable, SeasonTableMessage msg ->
        let nextState =
            model.SeasonTableState
            |> Option.map (SeasonTableApp.update msg) 
        {model with SeasonTableState = nextState}, []
        
    | GameDay, GameDayPageMessage msg ->
        let nextState, cmds =
            match model.GameDayState with
            | Some state ->
                let ns, cmds = GameDayApp.update msg state
                Some ns, cmds
                
            | None -> None, [] 
        {model with GameDayState = nextState}, cmds |> List.map GameDayPageCmd
    
    | _, SwitchPage newPageMessage ->
        match newPageMessage with
        | ToMainPage -> {model with Window = AppWindow.Main}, []
        | ToSeasonTablePage st ->
            {
                model with
                    Window = AppWindow.SeasonTable
                    SeasonTableState = Some <| SeasonTableApp.initModel st
            }, []
        | ToGameDayPage gd ->
            {
                model with
                    Window = AppWindow.GameDay;
                    GameDayState = Some <| GameDayApp.init gd
            }, []
        
    | _ -> failwithf "Unexpected state and message"
    
    
let validateGameDay = NoEmptyString.ofString 
let validateUrl = Url.create

let bindings () : Binding<Model, Message> list =
    
    [
        "Url" |> Binding.twoWayValidate(
            (fun m -> m.FirstPageState.TableUrl),
            (fun newUrl -> newUrl |> MainApp.TableUrlEntered |> MainPageMessage),
            (fun model -> model.FirstPageState.TableUrl |> validateUrl))

        "Day" |> Binding.twoWayValidate(
            (fun model -> model.FirstPageState.Day),
            (fun day -> day |> MainApp.GameDayEntered |> MainPageMessage),
            (fun m -> m.FirstPageState.Day |> validateGameDay))

        "LoadGameDay" |> Binding.cmdIf(
                                    fun model -> 
                                        result {
                                            let! url = validateUrl model.FirstPageState.TableUrl
                                            let! day = validateGameDay model.FirstPageState.Day

                                            return MainPageMessage <| MainApp.GameDayRequested(url, day)
                                        })
    
        "LoadSeasonTable" |> Binding.cmdIf(
            fun model ->
                model.FirstPageState.TableUrl
                |> Url.create
                |> Result.map (MainApp.SeasonTableRequested >> MainPageMessage))
        
        "ErrorMessage" |> Binding.oneWay(fun model -> model.FirstPageState.ErrorMessage |> Option.defaultValue "")
        
        
        "SeasonTableWin" |> Binding.subModelWin(
            (fun m -> match m.Window, m.SeasonTableState with SeasonTable, Some st -> WindowState.Visible st | _ -> WindowState.Closed), 
            snd, 
            id,
            SeasonTableApp.bindings SeasonTableMessage,
            (fun () -> SixtySeconds.Views.SeasonTableWindow(Owner = Application.Current.MainWindow)),
            onCloseRequested = SwitchPage ToMainPage,
            isModal = true
          )

        "GameDayWindow" |> Binding.subModelWin(
            (fun m -> match m.Window, m.GameDayState with GameDay, Some gd -> WindowState.Visible gd | _ -> WindowState.Closed), 
            snd, 
            id,
            GameDayApp.bindings GameDayPageMessage,
            (fun () -> SixtySeconds.Views.GameDayWindow(Owner = Application.Current.MainWindow)),
            onCloseRequested = SwitchPage ToMainPage,
            isModal = true
    )]
    

let private createWindow() = 
    
    let window = MainWindow()
    window
    
let toSeasonPage st = st |> ToSeasonTablePage |> SwitchPage
let toGamedayPage gd = gd |> ToGameDayPage |> SwitchPage
    
let toCmd = function
    | MainPageCmd cmd ->
        MainApp.toCmd cmd MainPageMessage toSeasonPage toGamedayPage
    | GameDayPageCmd cmd ->
        GameDayApp.toCmd cmd GameDayPageMessage
        
    

[<EntryPoint; STAThread>]
let main _ =

    let mainWindow = createWindow()
    
    Program.mkProgramWpfWithCmdMsg init update bindings toCmd
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true; Measure = true }
        (mainWindow)