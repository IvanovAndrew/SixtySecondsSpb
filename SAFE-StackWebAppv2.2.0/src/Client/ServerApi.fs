module Client.ServerApi

open Fable.Remoting.Client
open Shared.Api

let sixtySecondsApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ISixtySecondsApi>

