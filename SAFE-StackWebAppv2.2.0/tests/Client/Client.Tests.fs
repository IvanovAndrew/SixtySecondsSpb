module Client.Tests

open Fable.Mocha

open Shared.Models

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

let client = testList "Client" [
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