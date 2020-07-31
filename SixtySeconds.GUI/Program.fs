module SixtySeconds.GUI

open System
open System.Windows

open Elmish
open Elmish.WPF

open GameDayApp
open SixtySeconds.Views
open SixtySeconds.Common.CommonTypes

open SixtySeconds.Domain

type AppWindow =
    | Main
    | SeasonTable
    | GameDay
    

type Model =
    {
        FirstPageState : UrlApp.Model
        SeasonTableState : SeasonTableApp.Model option
        GameDayState : GameDayApp.Model option
        
        Window : AppWindow
    }
    
type CmdMsg =
    | MainPageCmd of UrlApp.CmdMsg
    | GameDayPageCmd of GameDayApp.CmdMsg

let init() =

    { 
        FirstPageState = UrlApp.init() 
        SeasonTableState = None
        GameDayState = None
        
        Window = Main
    }, []
    
type NewPage =
    | ToMainPage
    | ToSeasonTablePage of SeasonTable
    | ToGameDayPage of GameDay

type Message =
    | UrlPageMessage of UrlApp.Message
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
    | Main, UrlPageMessage msg ->
        let nextState, s = UrlApp.update msg model.FirstPageState
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
        "UrlPageWin" |> Binding.subModel(
            (fun m -> m.FirstPageState),
            snd,
            UrlPageMessage,
            UrlApp.bindings)
        
        "SeasonTableWin" |> Binding.subModelWin(
            (fun m -> match m.Window, m.SeasonTableState with SeasonTable, Some st -> WindowState.Visible st | _ -> WindowState.Closed), 
            snd, 
            SeasonTableMessage,
            SeasonTableApp.bindings,
            (fun () -> SixtySeconds.Views.SeasonTableWindow(Owner = Application.Current.MainWindow)),
            onCloseRequested = SwitchPage ToMainPage,
            isModal = true
          )

        "GameDayWindow" |> Binding.subModelWin(
            (fun m -> match m.Window, m.GameDayState with GameDay, Some gd -> WindowState.Visible gd | _ -> WindowState.Closed), 
            snd, 
            GameDayPageMessage,
            GameDayApp.bindings,
            (fun () -> SixtySeconds.Views.GameDayWindow(Owner = Application.Current.MainWindow)),
            onCloseRequested = SwitchPage ToMainPage,
            isModal = true
    )]
    

let private createWindow() = 
    
    let window = SixtySeconds.Views.MainWindow()
    window
    
let toSeasonPage st = st |> ToSeasonTablePage |> SwitchPage
let toGamedayPage gd = gd |> ToGameDayPage |> SwitchPage
    
let toCmd = function
    | MainPageCmd cmd ->
        UrlApp.toCmd cmd UrlPageMessage toSeasonPage toGamedayPage
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