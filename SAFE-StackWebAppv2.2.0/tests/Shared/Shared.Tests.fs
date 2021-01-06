module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared.Models

let firstTeam = {Id = 1; Name = "Team 1"}
let secondTeam = {Id = 2; Name = "Team 2"}
let thirdTeam = {Id = 3; Name = "Team 3"}
let gameday : GameDayModel =
        {
            Tournament =
                {
                    City = "City"
                    League = "League"
                    Season = "Season"
                }
            Name = "Test"
            Answers =
                [
                    firstTeam, [{Number = 1; Answer = true;}; {Number = 2; Answer = true;}; {Number = 3; Answer = false;};]
                    secondTeam, [{Number = 1; Answer = false;}; {Number = 2; Answer = true;}; {Number = 3; Answer = false;};]
                    thirdTeam, [{Number = 1; Answer = true;}; {Number = 2; Answer = true;}; {Number = 3; Answer = true;};]
                ] |> Map.ofSeq
            PackageSize = 3
        }

let shared = testList "Shared" [
    testCase "Gameday teams" <| fun _ ->
        let actual = gameday |> GameDay.teams |> Seq.length
        Expect.equal actual 3 "Should be three teams"
]