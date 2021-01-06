module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared.Models
open Shared.Api

open SixtySeconds.Common.CommonTypes
open SixtySeconds.Infrastructure


let sixtySecondsApi =
    {
        parseGameDay = fun url -> SixtySecondsApi.parseGameDay (url |> Url.create |> Result.valueOrException, NoEmptyString.ofConstString "13.10.2020")
        teamPerformance = fun (gameDay, team) -> SixtySecondsApi.teamPerformance (gameDay, team)
        parseSeasonRating = fun url -> SixtySecondsApi.parseTotal (url |> Url.create |> Result.valueOrException)
        filterRating = fun (options, seasonResult) -> SixtySecondsApi.filterTotalTable (options, seasonResult)
        writeToSpreadsheet = fun (props, gameDay, team) -> SixtySecondsApi.writeToSpreadsheet (props, gameDay, team)
        showChart = fun (chartType, gameDay) -> SixtySecondsApi.showChart (chartType, gameDay)
    }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue sixtySecondsApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app