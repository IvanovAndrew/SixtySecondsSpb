module Client.Tests

open Fable.Mocha

open Shared.Models
open System

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
                    {Id = 1; Name = "Team 1"}, [{Number = 1; Answer = true;}; {Number = 2; Answer = true;}; {Number = 3; Answer = false;};]
                    {Id = 2; Name = "Team 2"}, [{Number = 1; Answer = false;}; {Number = 2; Answer = true;}; {Number = 3; Answer = true;};]
                ] |> Map.ofSeq
            PackageSize = 3
        }
        
let seasonTableModel : SeasonResultModel =
    
        [
            {Id = 1; Name = "Team 1"}, [{Date = DateTime(2020, 11, 1); Points = 10m}; {Date = DateTime(2020, 12, 1); Points = 2m};{Date = DateTime(2020, 12, 15); Points = 3m};]
            {Id = 2; Name = "Team 2"}, [{Date = DateTime(2020, 11, 1); Points = 5m}; {Date = DateTime(2020, 12, 1); Points = 5m};{Date = DateTime(2020, 12, 15); Points = 5m};]
            {Id = 3; Name = "Team 3"}, [{Date = DateTime(2020, 11, 1); Points = 2m}; {Date = DateTime(2020, 12, 1); Points = 3m};{Date = DateTime(2020, 12, 15); Points = 5m};]
        ]
        |> Map.ofList

let client = testList "Client" [
    
    testCase "Bad url entered" <| fun _ ->
        let initialModel, _ = MainPage.init()
        
        let model, _ = MainPage.update (MainPage.UrlEntered "localhost") initialModel
        
        let expected = Error "Unknown url type" 
        Expect.equal model.Status expected "localhost should not be accepted"
        
    testCase "Game url entered" <| fun _ ->
        
        let model, _ =
            MainPage.init()
            |> fst
            |> MainPage.update (MainPage.UrlEntered "https://60sec.online/game/*")
        
        let expected = Ok "Game url entered" 
        Expect.equal model.Status expected "https://60sec.online/game/ should be accepted"
        
    testCase "Season url entered" <| fun _ ->
        let model, _ =
            MainPage.init()
            |> fst
            |> MainPage.update (MainPage.UrlEntered "https://60sec.online/season/*")
        
        let expected = Ok "Season url entered" 
        Expect.equal model.Status expected "https://60sec.online/season/ should be accepted"
        
    testCase "Url cleared" <| fun _ ->
        let model, _ =
            MainPage.init()
            |> fst
            |> MainPage.update (MainPage.UrlEntered "https://60sec.online/season/*")
            |> fst
            |> MainPage.update MainPage.UrlCleared
        
        let expected = Ok "" 
        Expect.equal model.Status expected ""
    
    testCase "Open spreadsheet window" <| fun _ ->
        
        let model = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenWriteToSpreadsheet) model

        let spreadsheetWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.Spreadsheet _) -> true
            | _ -> false
        
        Expect.isTrue spreadsheetWindowOpened "Spreadsheet window should be opened"
        
    testCase "Close spreadsheet window" <| fun _ ->
        
        let initialModel = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenWriteToSpreadsheet) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseWriteToSpreadsheet) model
        
        Expect.equal None updatedModel.ModalWindow "Spreadsheet window should be closed"
        
    testCase "Open team performance window" <| fun _ ->
        
        let initialModel = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenTeamPerformance) initialModel

        let teamPerformanceWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.TeamPerformance _) -> true
            | _ -> false
        
        Expect.isTrue teamPerformanceWindowOpened "Team performance window should be opened"
        
    testCase "Close team performance window" <| fun _ ->
        
        let initialModel = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenTeamPerformance) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseTeamPerformance) model
        
        Expect.equal None updatedModel.ModalWindow "Team performance window should be closed"
        
    testCase "Open chart window" <| fun _ ->
        
        let initialModel = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenChartWindow) initialModel

        let chartWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.Chart _) -> true
            | _ -> false
        
        Expect.isTrue chartWindowOpened "Chart window should be opened"
        
    testCase "Close chart window" <| fun _ ->
        
        let initialModel = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenChartWindow) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseChartWindow) model
        
        Expect.equal None updatedModel.ModalWindow "Chart window should be closed"
        
    testCase "Season table: final game counts towards season by default" <| fun _ ->
        
        let initialModel, _ = SeasonInfoPage.init seasonTableModel
        
        let expected = FinalGameCounts
        Expect.equal initialModel.Filter.RatingOption expected "Final games should be counted by default"
        
    testCase "Season table: final game has not played by default" <| fun _ ->
        
        let initialModel, _ = SeasonInfoPage.init seasonTableModel
        
        let expected = NotPlayedYet 
        Expect.equal initialModel.Filter.FinalDate expected "Final games should be not played by default"
        
    testCase "Season table has maximum games to count by default" <| fun _ ->
            
        let initialModel, _ = SeasonInfoPage.init seasonTableModel
        
        Expect.equal initialModel.MaximumGames initialModel.Filter.GamesToCount  "Season table has maximum games to count by default"
        
    testCase "Season table: when final game doesn't count then maximum games is lesser by one" <| fun _ ->
            
        let model = 
            seasonTableModel
            |> SeasonInfoPage.init
            |> fst
            |> SeasonInfoPage.update (SeasonInfoPage.Message.FinalGameCountsChanged(false))
            |> fst
        
        Expect.equal model.MaximumGames 2 "Season table has maximum games to count by default"
]

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Shared.Tests.shared
#endif
            client
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all