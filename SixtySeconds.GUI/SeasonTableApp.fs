module SeasonTableApp

open System
open System.Windows
open SixtySeconds.Domain
open SixtySeconds.Actions

open SixtySeconds.Common.CommonTypes

open Elmish.WPF

let topNTeams = 12

type TeamSeasonRating = 
    {
        Id : int
        Place : int
        Name : string
        Rating : decimal<Point>
        
        Team : Team
    }

type Model = 
    {
        GamesToCount : PositiveNum
        MaximumGames : PositiveNum
        SeasonTable : SeasonTable
        FilteredSeasonTable : TeamSeasonRating seq
        Playoff : string option
    }


    
let initModel (seasonTable : SeasonTable) = 
    { 
        GamesToCount = seasonTable.GamesCount
        MaximumGames = seasonTable.GamesCount
        SeasonTable = seasonTable
        FilteredSeasonTable = Seq.empty
        Playoff = None
    }
    
type CmdMsg =
    | CopyToClipboard of TeamSeasonRating seq 

let gamesToCountChanged gamesToCount model = 
    match gamesToCount |> PositiveNum.ofInt with
    | Ok games -> {model with GamesToCount = games}
    | Error _ -> model

let validateGamesToCount (seasonTable : SeasonTable) games = 
        
    let checkRange num = 
        if num <= seasonTable.GamesCount then Ok num
        else Error "Season has less games"
    
    games 
    |> PositiveNum.ofString
    |> Result.bind checkRange

let showTable count model = 
    let filtered = 
        let teamRating i (team, rating, place) = 
            {
                Id = team.ID.Value
                Name = team.Name.Value
                Place = i + 1; 
                Rating = rating
                Team = team
            }

        model.SeasonTable
        |> SeasonTable.topNResult count
        |> Seq.mapi teamRating

    {model with FilteredSeasonTable = filtered; Playoff = None}
    
let showPlayoff rating model =
    
    let playoffString = 
        rating
        |> Seq.take topNTeams
        |> Seq.map (fun r -> r.Team)
        |> Playoff.playoffString
        
    { model with Playoff = Some playoffString }
    
let asyncCopyToClipboard rating =
    
    async {
        rating
        |> Seq.map (fun row -> sprintf "%d. %s %.2f" row.Place row.Name <| Converter.toDecimal row.Rating)
        |> Seq.reduce (fun acc string -> sprintf "%s%s%s" acc Environment.NewLine string)
        |> Clipboard.SetText
    }

type Message = 
    | GamesToCountChanged of gamesToCount : int
    | ShowSeasonTable of count : PositiveNum
    | ShowPlayoff of TeamSeasonRating seq
    | CopyToClipboardRequested of TeamSeasonRating seq
    | CopiedToClipboard

let update message model = 
    match message with
    | GamesToCountChanged gamesToCount -> gamesToCountChanged gamesToCount model, []
    | ShowSeasonTable count  -> showTable count model, []
    | ShowPlayoff rating -> showPlayoff rating model, []
    | CopyToClipboardRequested rating -> model, [CopyToClipboard rating]
    | CopiedToClipboard -> model, []

let bindings() = 
    [
        "GamesToCount" |> Binding.twoWay(
            (fun m -> m.GamesToCount.Value |> float),
            (fun g m -> g |> int |> GamesToCountChanged)
            )
        "MaxValue" |> Binding.oneWay(fun m -> m.MaximumGames.Value |> float)
        "ShowSeasonTable" |> Binding.cmd(
                fun model -> 
                    model.GamesToCount
                    |> ShowSeasonTable 
            )
        "ShowPlayOff" |> Binding.cmdIf(
                fun model ->
                    model.FilteredSeasonTable
                    |> (fun v -> if Seq.isEmpty v then Error () else Ok v)
                    |> Result.map ShowPlayoff
                    )
        
        "CopyToClipboard" |> Binding.cmdIf(
                fun model ->
                        model.FilteredSeasonTable
                        |> (fun v -> if Seq.isEmpty v then Error() else Ok v)
                        |> Result.map CopyToClipboardRequested
                    )
        
        "Playoff" |> Binding.oneWay (fun model -> model.Playoff |> Option.defaultValue "")
        
        "FilteredSeasonTable" |> Binding.subModelSeq(
            (fun m -> m.FilteredSeasonTable),
            (fun item -> item.Id),
            (fun () -> [
                "Place" |> Binding.oneWay (fun (_, item) -> item.Place)
                "TeamName" |> Binding.oneWay (fun (_, item) -> item.Name)
                "Rating" |> Binding.oneWay (fun (_, item) -> item.Rating)
                ]))
    ]
    
let toCmd cmdMsg =
    
    match cmdMsg with
    | CopyToClipboard rating ->
        
        let ofSuccess _ = CopiedToClipboard 
        
        Elmish.Cmd.OfAsync.perform asyncCopyToClipboard rating ofSuccess