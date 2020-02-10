module SeasonTableApp

open System
open System.Windows
open Domain
open Utils
open Elmish.WPF

let topNTeams = 12

type TeamSeasonRating = 
    {
        Id : int
        Place : int
        Name : string
        Rating : decimal<Domain.Point>
        
        Team : Team
    }

type Model = 
    {
        GamesToCount : string
        SeasonTable : SeasonTable
        FilteredSeasonTable : TeamSeasonRating seq
        Playoff : string option
    }


    
let initModel (seasonTable : SeasonTable) = 
    { 
        GamesToCount = seasonTable.GamesCount |> PositiveNum.value |> string 
        SeasonTable = seasonTable
        FilteredSeasonTable = Seq.empty
        Playoff = None
    }

let gamesToCountChanged gamesToCount model = 
    {model with GamesToCount = gamesToCount}

let validateGamesToCount (seasonTable : SeasonTable) games = 
        
    let checkRange num = 
        if num <= seasonTable.GamesCount then Ok num
        else Error "Season has less games"
    
    games 
    |> PositiveNum.ofString
    |> Result.bind checkRange


let showTable count model = 
    let filtered = 
        let teamRating i (team, rating) = 
            {
                Id = team.ID |> Utils.PositiveNum.value
                Name = team.Name |> Utils.NoEmptyString.value
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
    
let copyToClipboard rating model =
    
    rating
    |> Seq.map (fun row -> sprintf "%d. %s %.2f" row.Place row.Name <| Converter.toDecimal row.Rating)
    |> Seq.reduce (fun acc string -> sprintf "%s%s%s" acc Environment.NewLine string)
    |> Clipboard.SetText
    
    model

type Message = 
    | GamesToCountChanged of gamesToCount : string
    | ShowSeasonTable of count : Utils.PositiveNum.PositiveNum
    | ShowPlayoff of TeamSeasonRating seq
    | CopyToClipboard of TeamSeasonRating seq

let update message model = 
    match message with
    | GamesToCountChanged gamesToCount -> gamesToCountChanged gamesToCount model
    | ShowSeasonTable count  -> showTable count model
    | ShowPlayoff rating -> showPlayoff rating model
    | CopyToClipboard rating -> copyToClipboard rating model

let bindings wrap = 
    (fun () -> [
        "GamesToCount" |> Binding.twoWayValidate(
            (fun m -> m.GamesToCount),
            (fun gamesToCount model -> wrap(GamesToCountChanged gamesToCount)),
            (fun m -> m.GamesToCount |> validateGamesToCount m.SeasonTable)
            )
        "ShowSeasonTable" |> Binding.cmdIf(
                fun model -> 
                    model.GamesToCount 
                    |> validateGamesToCount model.SeasonTable 
                    |> Result.map (ShowSeasonTable >> wrap)
            )
        "ShowPlayOff" |> Binding.cmdIf(
                fun model ->
                    model.FilteredSeasonTable
                    |> (fun v -> if Seq.isEmpty v then Error () else Ok v)
                    |> Result.map (ShowPlayoff >> wrap)
                    )
        
        "CopyToClipboard" |> Binding.cmdIf(
                fun model ->
                        model.FilteredSeasonTable
                        |> (fun v -> if Seq.isEmpty v then Error() else Ok v)
                        |> Result.map (CopyToClipboard >> wrap)
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
    ])