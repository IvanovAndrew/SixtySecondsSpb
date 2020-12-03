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
    | SeasonInfoPage of SeasonInfoPage.State

type UrlKind =
    | Game of string
    | Season of string

type State =
    {
        Url : UrlKind option
        Status : string
        Page : Page
    }

type Message =
    | UrlEntered of string
    | ButtonPressed
    | GameLoaded of GameDayModel
    | GameNotLoaded of string
    | SeasonTableLoaded of SeasonTableModel
    | SeasonTableNotLoaded of string
    | SeasonInfoMessage of SeasonInfoPage.Message
    | GameDayInfoMessage of GameDayPage.Message

let init(): State * Cmd<Message> =
    {
        Url = None
        Status = ""
        Page = UrlPage
    }, []

let update message state =
    match message, state.Page with
    | UrlEntered s, _ ->
        let url =
            if s.StartsWith("https://60sec.online/season") then Some <| Season s
            elif s.StartsWith("https://60sec.online/game") then Some <| Game s
            else None
        {state with Url = url; Status = ""}, Cmd.none
    | ButtonPressed, _ ->
        match state.Url with
        | Some site ->

            match site with
            | Season s ->
                let ofSuccess res =
                    match res with
                    | Ok value -> SeasonTableLoaded value
                    | Error str -> SeasonTableNotLoaded str

                let ofError exn = SeasonTableNotLoaded <| string exn

                state, Cmd.OfAsync.either sixtySecondsApi.parseSeasonRating s ofSuccess ofError

            | Game gameUrl ->
                let ofSuccess res =
                    match res with
                    | Ok value -> GameLoaded value
                    | Error str -> GameNotLoaded str
                let ofError exn = GameNotLoaded <| string exn
                state, Cmd.OfAsync.either sixtySecondsApi.parseGameDay gameUrl ofSuccess ofError
        | None -> state, Cmd.none
    | GameLoaded gd, _ -> {state with Page = GameInfoPage <| GameDayPage.init gd }, Cmd.none
    | GameNotLoaded e, _ -> {state with Status = e}, Cmd.none
    | SeasonTableLoaded st, _ -> {state with Page = SeasonInfoPage <| SeasonInfoPage.init st}, Cmd.none
    | SeasonTableNotLoaded e, _ -> {state with Status = e}, Cmd.none

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

        let buttonText =
            match state.Url with
            | Some v ->
                match v with
                | Game _ -> "Show game info"
                | Season _ -> "Show season info"
            | None -> "Show season info"

        div []
            [
                Message.message []
                    [
                        str "Url"
                        Input.text
                                [
                                    Input.Placeholder "Enter url like this https://60sec.online/season/1031/"
                                    Input.OnChange (fun x -> x.Value |> UrlEntered |> dispatch)
                                ]
                        Help.help [Help.Color IsDanger] [str state.Status]
                        Button.button
                                [
                                    Button.Disabled (state.Url |> Option.isNone)
                                    Button.OnClick (fun x -> ButtonPressed |> dispatch)
                                ]
                                [str buttonText]
                    ]
            ]

    | SeasonInfoPage state -> SeasonInfoPage.render state dispatchSeasonInfoPage
    | GameInfoPage state -> GameDayPage.render state dispatchGameInfoPage