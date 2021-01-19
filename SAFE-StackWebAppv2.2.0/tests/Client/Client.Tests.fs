module Client.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

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
        
let secondsSeason : SixtySecondsSeasonModel =
    
        [
            {Id = 1; Name = "Team 1"}, [{Date = DateTime(2020, 11, 1); Points = 10m}; {Date = DateTime(2020, 12, 1); Points = 2m};{Date = DateTime(2020, 12, 15); Points = 3m};]
            {Id = 2; Name = "Team 2"}, [{Date = DateTime(2020, 11, 1); Points = 5m}; {Date = DateTime(2020, 12, 1); Points = 5m};{Date = DateTime(2020, 12, 15); Points = 5m};]
            {Id = 3; Name = "Team 3"}, [{Date = DateTime(2020, 11, 1); Points = 2m}; {Date = DateTime(2020, 12, 1); Points = 3m};{Date = DateTime(2020, 12, 15); Points = 5m};]
        ]
        |> Map.ofList

let matrixSeason : MatrixSeasonModel =
    
        [
            {Id = 1; Name = "Team 1"}, [{Date = DateTime(2020, 11, 1); Points = 50m}; {Date = DateTime(2020, 12, 1); Points = 20m};{Date = DateTime(2020, 12, 15); Points = 30m};]
            {Id = 2; Name = "Team 2"}, [{Date = DateTime(2020, 11, 1); Points = 50m}; {Date = DateTime(2020, 12, 1); Points = 50m};{Date = DateTime(2020, 12, 15); Points = 50m};]
            {Id = 3; Name = "Team 3"}, [{Date = DateTime(2020, 11, 1); Points = 20m}; {Date = DateTime(2020, 12, 1); Points = 30m};{Date = DateTime(2020, 12, 15); Points = 50m};]
        ]
        |> Map.ofList

[<Tests>]
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
        
        let model, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenWriteToSpreadsheet) model

        let spreadsheetWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.Spreadsheet _) -> true
            | _ -> false
        
        Expect.isTrue spreadsheetWindowOpened "Spreadsheet window should be opened"
        
    testCase "Close spreadsheet window" <| fun _ ->
        
        let initialModel, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenWriteToSpreadsheet) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseWriteToSpreadsheet) model
        
        Expect.equal None updatedModel.ModalWindow "Spreadsheet window should be closed"
        
    testCase "Open team performance window" <| fun _ ->
        
        let initialModel, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenTeamPerformance) initialModel

        let teamPerformanceWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.TeamPerformance _) -> true
            | _ -> false
        
        Expect.isTrue teamPerformanceWindowOpened "Team performance window should be opened"
        
    testCase "Close team performance window" <| fun _ ->
        
        let initialModel, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenTeamPerformance) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseTeamPerformance) model
        
        Expect.equal None updatedModel.ModalWindow "Team performance window should be closed"
        
    testCase "Open chart window" <| fun _ ->
        
        let initialModel, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenChartWindow) initialModel

        let chartWindowOpened = 
            match model.ModalWindow with
            | Some (GameDayPage.Chart _) -> true
            | _ -> false
        
        Expect.isTrue chartWindowOpened "Chart window should be opened"
        
    testCase "Close chart window" <| fun _ ->
        
        let initialModel, _ = GameDayPage.init gameday

        let model, _ = GameDayPage.update (GameDayPage.OpenChartWindow) initialModel

        let updatedModel, _ = GameDayPage.update (GameDayPage.CloseChartWindow) model
        
        Expect.equal None updatedModel.ModalWindow "Chart window should be closed"
        
    testCase "Season table: final game counts towards season by default" <| fun _ ->
        
        let initialModel, _ = SeasonInfoPage.init secondsSeason matrixSeason
        
        let option =
            match initialModel.ActiveSubmodel with
            | SeasonInfoPage.SixtySeconds f -> f.Filter.RatingOption
            | SeasonInfoPage.Matrix f -> FinalGameDoesntCount
        
        let expected = FinalGameCounts
        Expect.equal option expected "Final games should be counted by default"
        
    testCase "Season table: final game has not played by default" <| fun _ ->
        
        let initialModel, _ = SeasonInfoPage.init secondsSeason matrixSeason
        
        let actual =
            match initialModel.ActiveSubmodel with
            | SeasonInfoPage.SixtySeconds f -> f.Filter.FinalDate
            | SeasonInfoPage.Matrix f -> NotPlayedYet
         
        Expect.equal actual NotPlayedYet "Final games should be not played by default"
        
    testCase "Season table has maximum games to count by default" <| fun _ ->
            
        let initialModel, _ = SeasonInfoPage.init secondsSeason matrixSeason
        
        let expected, actual =
            match initialModel.ActiveSubmodel with
            | SeasonInfoPage.SixtySeconds f -> f.Filter.GamesToCount, f.MaximumGames
            | SeasonInfoPage.Matrix f -> f.Filter.GamesToCount, f.MaximumGames
        
        Expect.equal actual expected "Season table has maximum games to count by default"
        
    testCase "Season table: when final game doesn't count then maximum games is lesser by one" <| fun _ ->
            
        let model = 
            matrixSeason
            |> SeasonInfoPage.init secondsSeason
            |> fst
            |> SeasonInfoPage.update (SeasonInfoPage.FinalGameCountsChanged(false))
            |> fst
        
        let actual =
            match model.ActiveSubmodel with
            | SeasonInfoPage.SixtySeconds f -> f.MaximumGames
            | SeasonInfoPage.Matrix f -> f.MaximumGames
        
        Expect.equal actual 2 "Season table has maximum games to count by default"
        
    testCase "Season table: 60 seconds table is chosen by default" <| fun _ ->
            
        let model = 
            matrixSeason
            |> SeasonInfoPage.init secondsSeason
            |> fst
            
        let is60secTab =
            match model.ActiveSubmodel with
            | SeasonInfoPage.Matrix  _ -> true
            | SeasonInfoPage.SixtySeconds _ -> false
        
        Expect.isTrue is60secTab "Season table has chosen 60 seconds tab by default"
        
    testCase "Season table: choose matrix tab" <| fun _ ->
            
        let model = 
            matrixSeason
            |> SeasonInfoPage.init secondsSeason
            |> fst
            |> SeasonInfoPage.update SeasonInfoPage.TabChanged
            |> fst
        
        let isMatrixTab =
            match model.ActiveSubmodel with
            | SeasonInfoPage.Matrix  _ -> false
            | SeasonInfoPage.SixtySeconds _ -> true
        
        Expect.isTrue isMatrixTab "Must be matrix"
        
    (*testCase "Season table: changing tab updates table" <| fun _ ->
            
        let cmd = 
            matrixSeason
            |> SeasonInfoPage.init secondsSeason
            |> fst
            |> SeasonInfoPage.update (SeasonInfoPage.TabChanged(SeasonInfoPage.SeasonTab.Matrix))
            |> snd
        
        
        Expect.notEqual cmd Elmish.Cmd.none "Must be command"
        *)
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
let main args =
#if FABLE_COMPILER
    Mocha.runTests all
#else
    runTestsWithArgs defaultConfig args all
#endif