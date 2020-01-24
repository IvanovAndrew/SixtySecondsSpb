module SeasonTableApp

open Domain
open Utils
open Elmish.WPF

type TeamSeasonRating = 
    {
        Id : int
        Place : int
        Name : string
        Rating : decimal<Domain.Point>
    }

type Model = 
    {
        GamesToCount : string
        SeasonTable : SeasonTable
        FilteredSeasonTable : TeamSeasonRating seq
    }


    
let initWindow (seasonTable : SeasonTable) = 
    { 
        GamesToCount = seasonTable.GamesCount |> PositiveNum.value |> string; 
        SeasonTable = seasonTable; 
        FilteredSeasonTable = Seq.empty
    }

let gamesToCountChanged gamesToCount window = 
    {window with GamesToCount = gamesToCount}

let validateGamesToCount (seasonTable : SeasonTable) games = 
        
    let checkRange num = 
        if num <= seasonTable.GamesCount then Ok num
        else Error "Season has less games"
    
    games 
    |> PositiveNum.ofString
    |> Result.bind checkRange


let showTable count window = 
    let filtered = 
        let teamRating i (team, rating) = 
            {
                Id = team.ID |> Utils.PositiveNum.value
                Name = team.Name |> Utils.NoEmptyString.value
                Place = i + 1; 
                Rating = rating
            }

        window.SeasonTable
        |> SeasonTable.topNResult count
        |> Seq.mapi teamRating

    {window with FilteredSeasonTable = filtered}

type Message = 
    | GamesToCountChanged of gamesToCount : string
    | ShowSeasonTable of count : Utils.PositiveNum.PositiveNum

let update message model = 
    match message with
    | GamesToCountChanged gamesToCount -> gamesToCountChanged gamesToCount model
    | ShowSeasonTable count  -> showTable count model

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
        
        "FilteredSeasonTable" |> Binding.subModelSeq(
            (fun m -> m.FilteredSeasonTable),
            (fun item -> item.Id),
            (fun () -> [
                "Place" |> Binding.oneWay (fun (_, item) -> item.Place)
                "TeamName" |> Binding.oneWay (fun (_, item) -> item.Name)
                "Rating" |> Binding.oneWay (fun (_, item) -> item.Rating)
                ]))
    ])