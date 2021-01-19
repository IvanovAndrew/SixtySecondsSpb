module MainPage

open Client
open Elmish
open Elmish.React

open Fable.React
open Fable.Remoting.Client
open Fulma

open ServerApi

open Shared.Models

type Page =
    | UrlPage
    | GameInfoPage of GameDayPage.State
    | SeasonInfoPage of SeasonInfoPage.Model

type UrlKind =
    | Game of string
    | Season of string

type State =
    {
        Url : UrlKind option
        Status : Result<string, string>
        Page : Page
    }
    
let private checkUrl (url : string) =
    
    if url.StartsWith("https://60sec.online/season") then url |> Season |> Ok
    elif url.StartsWith("https://60sec.online/game") then url |> Game |> Ok
    else Error "Unknown url type"

type Message =
    | UrlEntered of string
    | UrlCleared
    | ButtonPressed
    | GameLoaded of GameDayModel
    | GameNotLoaded of string
    | SeasonResultLoaded of SixtySecondsSeasonModel * MatrixSeasonModel
    | SeasonResultNotLoaded of string
    | SeasonInfoMessage of SeasonInfoPage.Message
    | GameDayInfoMessage of GameDayPage.Message

let init(): State * Cmd<Message> =
    {
        Url = None
        Status = Ok ""
        Page = UrlPage
    }, []

let update message state =
    match message, state.Page with
    | UrlEntered s, _ ->
        match checkUrl s with
        | Ok url ->
            let text =
                match url with
                | Game _ -> "Game url entered"
                | Season _ -> "Season url entered"
            {state with Url = Some url; Status = Ok text}, Cmd.none
        | Error e -> {state with Url = None; Status = Error e}, Cmd.none
    | UrlCleared, _ -> {state with Url = None; Status = Ok ""}, Cmd.none
    | ButtonPressed, _ ->
        match state.Url with
        | Some site ->

            match site with
            | Season s ->
                let ofSuccess res =
                    match res with
                    | Ok value -> SeasonResultLoaded value
                    | Error str -> SeasonResultNotLoaded str

                let ofError = string >> SeasonResultNotLoaded

                state, Cmd.OfAsync.either sixtySecondsApi.parseSeasonRating s ofSuccess ofError

            | Game gameUrl ->
                let ofSuccess res =
                    match res with
                    | Ok value -> GameLoaded value
                    | Error str -> GameNotLoaded str
                let ofError = string >> GameNotLoaded 
                state, Cmd.OfAsync.either sixtySecondsApi.parseGameDay gameUrl ofSuccess ofError
        | None -> state, Cmd.none
    | GameLoaded gd, _ ->
        let updatedState, nextCmd = GameDayPage.init gd
        {state with Page = GameInfoPage <| updatedState }, nextCmd |> Cmd.map GameDayInfoMessage
    | GameNotLoaded e, _ -> {state with Status = Error e}, Cmd.none
    | SeasonResultLoaded (seconds, matrix), _ ->
        let updatedState, nextCmd = SeasonInfoPage.init seconds matrix
        {state with Page = SeasonInfoPage updatedState}, nextCmd |> Cmd.map SeasonInfoMessage
    
    | SeasonResultNotLoaded e, _ -> {state with Status = Error e}, Cmd.none

    | SeasonInfoMessage message, SeasonInfoPage page ->

        let updatedState, nextCmd = SeasonInfoPage.update message page
        { state with Page = SeasonInfoPage updatedState }, nextCmd |> Cmd.map SeasonInfoMessage

    | GameDayInfoMessage message, GameInfoPage page ->

        let updatedState, nextCmd = GameDayPage.update message page
        { state with Page = GameInfoPage updatedState }, nextCmd |> Cmd.map GameDayInfoMessage

    | _ -> state, Cmd.none

let render (state : State) (dispatch: Message -> unit) =

    let dispatchSeasonInfoPage (msg : SeasonInfoPage.Message) = dispatch (SeasonInfoMessage msg)
    let dispatchGameInfoPage (msg : GameDayPage.Message) = dispatch (GameDayInfoMessage msg)

    match state.Page with
    | UrlPage ->

        let help =
            match state.Status with
            | Ok text -> Help.help [Help.Color IsSuccess] [str text]
            | Error error -> Help.help [Help.Color IsDanger] [str error]

        div []
            [
                Field.div []
                    [
                        Label.label [] [str "Url"]
                        Control.div []
                            [
                                Input.text
                                    [
                                        (match state.Status with Ok _ -> Input.Color NoColor | _ -> Input.Color IsDanger)
                                        Input.Placeholder "Enter url like this https://60sec.online/season/1031/"
                                        Input.OnChange (fun x -> x.Value |> (fun x -> if x = "" then UrlCleared else UrlEntered x) |> dispatch)
                                    ]
                                help
                            ]
                    ]
                Button.button
                    [
                        Button.Disabled (state.Url |> Option.isNone)
                        Button.OnClick (fun x -> ButtonPressed |> dispatch)
                    ]
                    [str "Show info"]
            ]

    | SeasonInfoPage state -> SeasonInfoPage.render state dispatchSeasonInfoPage
    | GameInfoPage state -> GameDayPage.render state dispatchGameInfoPage
