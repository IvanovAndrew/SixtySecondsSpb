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
        
    testCase "Team best place" <| fun _ ->
        let actual = Team.bestPlace gameday firstTeam
        Expect.equal actual {Place = {From = 1; To = 2;}; Question = 2} "Best place of first team is 1-2 after the second question"
        
    testCase "Team worst place" <| fun _ ->
        let actual = Team.worstPlace gameday firstTeam
        Expect.equal actual {Place = {From = 2; To = 2;}; Question = 3} "Worst place of first team is 2 after the third question"
        
    testCase "Team best strike" <| fun _ ->
        let actual = Team.bestStrike gameday firstTeam
        Expect.equal actual {Type = Best; Count = Some 2} "Best strike of the first team is equal to 2"
        
    testCase "Team worst strike" <| fun _ ->
        let actual = Team.worstStrike gameday firstTeam
        Expect.equal actual {Type = Worst; Count = Some 1} "Worst strike of the first team is equal to 1"
        
    testCase "Team difficult answered question" <| fun _ ->
        let actual = Team.difficultAnswered gameday firstTeam
        Expect.equal actual ({Number = 1; Answer = true}, 2) "The first team answered on the first question"
        
    testCase "Team simplest wrong answered question" <| fun _ ->
        let actual = Team.simplestWrongAnswered gameday firstTeam
        Expect.equal actual ({Number = 3; Answer = false}, 1) "The first team wrong answered on the third question"
]