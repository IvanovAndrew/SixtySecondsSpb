namespace Parser

open System
open FSharp.Data
open FSharp.Data
open FSharp.Data.JsonExtensions
open NUnit.Framework

[<TestFixture>]
module ParsingTests =
    
    open SixtySeconds.Common.CommonTypes
    open SixtySeconds.Common.Errors
    open SixtySeconds.Common.ErrorMessages
    
    let private createTableTitle dates =
        HtmlNode.NewElement("div", [],
                              dates
                              |> List.map(fun d -> HtmlNode.NewElement("div", [], [HtmlNode.NewText d]))
                              |> List.append
                                  [
                                      HtmlNode.NewElement("div", [], [HtmlNode.NewText "#"])
                                      HtmlNode.NewElement("div", [], [HtmlNode.NewText "Название"])
                                      HtmlNode.NewElement("div", [], [HtmlNode.NewText "Сумма"])
                                  ]
                            )
        
    let private createTeamLine number teamId name sum results =
        
        HtmlNode.NewElement("div", [],
                              results
                              |> List.map (fun res -> HtmlNode.NewElement("div", [], [HtmlNode.NewText (string res)]))
                              |> List.append
                                  [
                                    HtmlNode.NewElement("div", [], [HtmlNode.NewText number])
                                    HtmlNode.NewElement("div", [], [HtmlNode.NewElement("div", [("href", sprintf "/team/%s/" teamId)], [HtmlNode.NewText name])])
                                    HtmlNode.NewElement("div", [], [HtmlNode.NewText sum])
                                  ]
                              )
        

    let private createTableElement dates teamResults tableId  =
        HtmlNode.NewElement("div", [("id", tableId)],
                  [
                      HtmlNode.NewElement("div", [("id", "rate_table")],
                            [
                                HtmlNode.NewElement("div", [], [createTableTitle dates])
                                HtmlNode.NewElement("div", [], [createTeamLine "42" "10" "Команда" (teamResults |> List.sum |> string ) teamResults])
                            ])
                  ])
        
    let private createDocument children = HtmlDocument.New [HtmlNode.NewElement ("body", [], children)]
    
    [<Test>]
    let ``Parse correct total tables``() =
        
        let createTableElement' = createTableElement ["01.01.2021"; "02.01.2021"; "03.01.2021";] [10; 15; 5;]
        let document = createDocument [createTableElement' "sec"; createTableElement' "matrix"]
        
        let actual = 
            document
            |> Parser.parseTotalFrom60SecSite
            
        match actual with
        | Ok v -> Assert.Pass()
        | Error e -> Assert.Fail(e.ToString())
        
        
        match actual with
        | Ok v -> Assert.Pass()
        | Error e -> Assert.Fail(e.ToString())
        
        
    let private failWithErrorDescription e =
        e
        |> ParsingError
        |> errorToString
        |> (fun e -> Assert.Fail(e))
        
    [<Test>]
    let ``When 60 seconds table is not found then error will be returned``() =
        
        let createTableElement' = createTableElement ["01.01.2021"; "02.01.2021"; "03.01.2021";] [10; 15; 5;]
        let document = createDocument [createTableElement' "matrix"]
        
        let actual = 
            document
            |> Parser.parseTotalFrom60SecSite
            
        match actual with
        | Error e ->
            match e with
            | TableNotFound str when str = "sec" -> Assert.Pass()
            | _ -> failWithErrorDescription e
        | Ok _ -> Assert.Fail()
        
    [<Test>]
    let ``When matrix table is not found then error will be returned``() =
        
        let document =
            let createTableElement' = createTableElement ["01.01.2021"; "02.01.2021"; "03.01.2021";] [10; 15; 5;]
            createDocument [createTableElement' "sec"]
        
        let actual = 
            document
            |> Parser.parseTotalFrom60SecSite
            
        match actual with
        | Error e ->
            match e with
            | TableNotFound str when str = "matrix" -> Assert.Pass()
            | _ -> failWithErrorDescription e
        | Ok _ -> Assert.Fail()
        
        
    [<Test>]
    let ``Gameday: Can not parse date error``() =
        
        let document =
            let createTableElement' = createTableElement ["02012021";] [10;]
            createDocument [createTableElement' "sec"; createTableElement' "matrix"]
        
        let actual = 
            document
            |> Parser.parseTotalFrom60SecSite
            
        match actual with
        | Error e ->            
            match e with
            | CantParseDate str when str |> String.containsSubstring "02012021" -> Assert.Pass()
            | _ -> failWithErrorDescription e
        | Ok _ -> Assert.Fail()
        
        
    let private createTeamRecord teamId teamName answers =
        
        JsonValue.Record
            [|
              "team_id", JsonValue.Number (decimal teamId)
              "title", JsonValue.String teamName
              "newmask", JsonValue.Array (answers |> Array.map (decimal >>JsonValue.Number))
            |]
        
    [<Test>]
    let ``Gameday: parse table``() =
        let teamResults =
                    JsonValue.Array
                        [|
                            createTeamRecord 42 "Test team" [|1; 0; 1; 0|]
                        |]
                              
        let resultsAsString = teamResults.ToString()
        let json = JsonValue.Record [| "results", JsonValue.String resultsAsString |]
        
        let parse = 
            Parser.parse60SecondGameDay
                {City = NoEmptyString.ofConstString "Город золотой"; League = NoEmptyString.ofConstString "Лига теста"; Season = NoEmptyString.ofConstString "Тестовый сезон"} <| NoEmptyString.ofConstString "Игра 1" 
    
        match parse json with
        | Ok table -> Assert.Pass()
        | Error e -> failWithErrorDescription e
        
    [<Test>]
    let ``Gameday: every team should be unique``() =
        let teamResults =
                    JsonValue.Array
                        [|
                            createTeamRecord 42 "Test team" [|1; 0; 1; 0|]
                            createTeamRecord 42 "Test team" [|1; 1; 1; 1|]
                        |]
                              
        let resultsAsString = teamResults.ToString()
        let json = JsonValue.Record [| "results", JsonValue.String resultsAsString |]
        
        let parse = 
            Parser.parse60SecondGameDay
                {City = NoEmptyString.ofConstString "Город золотой"; League = NoEmptyString.ofConstString "Лига теста"; Season = NoEmptyString.ofConstString "Тестовый сезон"} <| NoEmptyString.ofConstString "Игра 1" 
    
        match parse json with
        | Ok table -> Assert.Fail("Table is parsed, but should failed")
        | Error e ->
            match e with
            | BusinessError err ->
                match err with
                | TeamAlreadyAdded _ -> Assert.Pass()
                | _ -> failWithErrorDescription e 
            | _ -> failWithErrorDescription e
            
            
    [<Test>]
    let ``Gameday: every team should have answers data``() =
        let teamResults =
                    JsonValue.Array
                        [|
                            createTeamRecord 42 "Test team" [||]
                        |]
                              
        let resultsAsString = teamResults.ToString()
        let json = JsonValue.Record [| "results", JsonValue.String resultsAsString |]
        
        let parse = 
            Parser.parse60SecondGameDay
                {City = NoEmptyString.ofConstString "Город золотой"; League = NoEmptyString.ofConstString "Лига теста"; Season = NoEmptyString.ofConstString "Тестовый сезон"} <| NoEmptyString.ofConstString "Игра 1" 
    
        match parse json with
        | Ok _ -> Assert.Fail("Table is parsed, but should failed")
        | Error e ->
            match e with
            | MissingAnswersCount -> Assert.Pass()
            | _ -> failWithErrorDescription e
            
    [<Test>]
    let ``Gameday: every team should have same answers count``() =
        let teamResults =
                    JsonValue.Array
                        [|
                            createTeamRecord 42 "Test team" [|1; 0; 1|]
                            createTeamRecord 42 "Test team" [|1; 0; 1; 1|]
                        |]
                              
        let resultsAsString = teamResults.ToString()
        let json = JsonValue.Record [| "results", JsonValue.String resultsAsString |]
        
        let parse = 
            Parser.parse60SecondGameDay
                {City = NoEmptyString.ofConstString "Город золотой"; League = NoEmptyString.ofConstString "Лига теста"; Season = NoEmptyString.ofConstString "Тестовый сезон"} <| NoEmptyString.ofConstString "Игра 1" 
    
        match parse json with
        | Ok _ -> Assert.Fail("Table is parsed, but should failed")
        | Error e ->
            match e with
            | BusinessError err ->
                match err with
                | QuestionsCountMismatching _ -> Assert.Pass()
                | _ -> failWithErrorDescription e
            | _ -> failWithErrorDescription e