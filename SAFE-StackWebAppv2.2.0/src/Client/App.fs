module App

open Elmish
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram MainPage.init MainPage.update MainPage.render
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "60-seconds-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run