module SixtySeconds.GUI

open System
open System.Text.RegularExpressions
open System.Windows

open Elmish
open Elmish.WPF

open SixtySecond.GUI.Settings
open SixtySeconds.Views

open Utils

let validateGameDay (s : string) = 
    if Regex.IsMatch(s, @"^\d{1,2}\.\d{1,2}(.\d{4})?") 
    then Ok s
    else Error "Incorrect game day"

let validateUrl = Url.create

type Model =
    { 
        TableUrl : string
        Day : string
        ErrorMessage : string option

        SeasonTableWin : SeasonTableApp.Model option
        GameDayWindow : GameDayApp.Model option
    }

let init() =

    { 
        TableUrl = Config.load TableUrl 
        Day = sprintf "%d.%02d" DateTime.Today.Day DateTime.Today.Month
        ErrorMessage = None

        SeasonTableWin = None
        GameDayWindow = None
    }

type Message =
    | TableUrlEntered of string
    | GameDayEntered of string
    | LoadGameDay of Url * string
    

    | GameDayMessage of GameDayApp.Message

    | GameDayCloseRequested

    | LoadSeasonTable of Url
    
    | SeasonTableMessage of SeasonTableApp.Message
    | SeasonTableCloseRequested
    


let update msg m =
    match msg with
    | TableUrlEntered url -> {m with TableUrl = url; ErrorMessage = None}
        
    | GameDayEntered d -> {m with Day = d; ErrorMessage = None}
    | LoadSeasonTable url -> 
        
        let seasonWindow = 
            async {
                let! document = url |> Url.value |> Parser.asyncLoadDocument 

                return 
                    document
                    |> Result.bind Parser.parseTotal
                    |> Result.map SeasonTableApp.initModel
            } |> Async.RunSynchronously
            
            
        match seasonWindow with 
        | Ok (window : SeasonTableApp.Model)->
            
            Config.save TableUrl m.TableUrl
            {m with SeasonTableWin = Some window}
        | Error e -> 
            {m with ErrorMessage = Some e}

    | LoadGameDay(url, gameName) -> 
        
        let gameDayResult = 
            
            async {
                
                let! document = url |> Url.value |> Parser.asyncLoadDocument 
                
                return document |> Result.bind (Parser.parse gameName)
            } |> Async.RunSynchronously
            
        match gameDayResult with 
        | Ok gameDay -> 
            {m with
                GameDayWindow = GameDayApp.init gameDay |> Some
            }
        | Error e -> {m with ErrorMessage = Some e}

    | GameDayMessage message -> 
        {
            m with 
                GameDayWindow = 
                    m.GameDayWindow |> Option.map (GameDayApp.update message)
            }
    | GameDayCloseRequested -> {m with GameDayWindow = None}
    
    | SeasonTableMessage message -> 
        {
            m with 
                SeasonTableWin = 
                    m.SeasonTableWin |> Option.map(SeasonTableApp.update message)
        }
    | SeasonTableCloseRequested -> {m with SeasonTableWin = None}
    

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
        (fun m -> m.SeasonTableWin |> WindowState.ofOption), 
        snd, 
        id,
        SeasonTableApp.bindings SeasonTableMessage,
        (fun () -> SixtySeconds.Views.SeasonTableWindow(Owner = Application.Current.MainWindow)),
        onCloseRequested = SeasonTableCloseRequested,
        isModal = true
      )

    "GameDayWindow" |> Binding.subModelWin(
        (fun m -> m.GameDayWindow |> WindowState.ofOption), 
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
    
    Program.mkSimpleWpf init update bindings
    |> Program.withConsoleTrace
    |> Program.runWindowWithConfig
        { ElmConfig.Default with LogConsole = true; Measure = true }
        (mainWindow)