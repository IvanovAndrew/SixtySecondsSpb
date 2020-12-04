module Parser

open System
open FSharp.Data
open FSharp.Data
open FSharp.Data
open FSharp.Data.JsonExtensions

open FSharp.Data
open SixtySeconds.Domain
open SixtySeconds.Actions

open SixtySeconds.Common.Errors
open SixtySeconds.Common.CommonTypes

let asyncLoadString url =
    async {
        let urlString = Url.value url
        let! response = Http.AsyncRequest(urlString, silentHttpErrors = true)
        
        let result = 
            if response.StatusCode <> 200 then urlString |> pageNotFound 
            else
                match response.Body with 
                | HttpResponseBody.Text text -> 
                    text
                    |> Ok
                | _ -> unexpectedResponse() 

        return result
    }

let asyncLoadDocument url = 
    
    async {
        let urlString = Url.value url
        let! response = Http.AsyncRequest(urlString, silentHttpErrors = true)

        let result = 
            if response.StatusCode <> 200 then urlString |> pageNotFound 
            else
                match response.Body with 
                | HttpResponseBody.Text text -> 
                    
                    if text.Contains("Ошибка \"404\"") then urlString |> pageNotFound
                    else 
                        text
                        |> String.replace "<style>@import url(https://fonts.googleapis.com/css?kit=o--8Et3j0xElSo4Jk-6CSN_pgL91BiSHK8etQbSopkk);</style>" "" 
                        |> HtmlDocument.Parse 
                        |> Ok
                | _ -> unexpectedResponse() 

        return result
    }

type ParserOption = 
    {
        IdColumn : int 
        NameColumn : int 
        SumColumn : int
        AnswersColumns : int list
    }

type RatingParserOption = 
    {
        TeamIdColumn : int 
        TeamNameColumn : int
        FirstResultColumn : int
        FinalResultColumn : int
    }

module HtmlNode = 
    
    let innerText (node : HtmlNode) = node.InnerText()
    let elements (node : HtmlNode) = node.Elements()
    let firstElement (node : HtmlNode) = node |> elements |> Seq.head
    let elementWithId id (node : HtmlNode) =
        node
        |> elements
        |> Seq.find (fun node -> node |> HtmlNode.hasId id)
    let descendants (node : HtmlNode) = node.Descendants()
    let attributes (node : HtmlNode) = node.Attributes()
    let attribute name (node : HtmlNode) = node.Attribute(name)
    let hasAttribute attribute value (node : HtmlNode) = node.HasAttribute(attribute, value)
    
//module HtmlDocument =
//    
//    let elements (document : HtmlDocument) = document |> document.Elements()

module Attribute = 
    
    let name (attribute : HtmlAttribute) = attribute.Name()
    let value (attribute : HtmlAttribute) = attribute.Value()

let private getSheetId (document : HtmlDocument) (sheetName : NoEmptyString) = 
    
    let children = 
        document.Elements()
        |> List.ofSeq
        |> List.head
        |> HtmlNode.descendants

    let nodeOption = 
        children
        |> Seq.tryFind (HtmlNode.innerText >> String.containsSubstring (sheetName.Value))
        
            
    match nodeOption with 
    | Some node -> 
        node
        |> HtmlNode.descendants
        |> Seq.filter (HtmlNode.innerText >> ((=) sheetName.Value))
        |> Seq.find (HtmlNode.attributes >> List.exists (Attribute.name >> ((=) "id")))
        |> HtmlNode.attribute "id"
        |> Attribute.value
        |> String.replace "sheet-button-" ""
        |> Ok
    | None ->
        sheetName.Value
        |> sheetNotFound
        
let private parseTeam idColumn nameColumn innerText =
            
    let teamId = idColumn |> innerText |> int
    let name = nameColumn |> innerText
    createTeam teamId name
    |> expectTeamParsingError

let private findSheetNode document sheetName = 
    
    let findNodeById sheetId =
        let elem = 
            document
            |> HtmlDocument.elements
            |> Seq.head

        // TODO rewrite
        elem
        |> HtmlNode.descendants
        |> Seq.find (HtmlNode.hasAttribute "id" sheetId)
        |> HtmlNode.firstElement
        |> HtmlNode.firstElement
        |> HtmlNode.elements
        |> Seq.tail
        |> Seq.head
    
    sheetName
    |> getSheetId document
    |> Result.map findNodeById

let private parseTitle document =
        
    document
    |> HtmlDocument.elements
    |> Seq.head
    |> HtmlNode.descendants
    |> Seq.find (fun node -> node.Name() = "title")
    |> HtmlNode.innerText
    
let private createGameDay gameDay parse teamsToParse = 
    
    let updateGameDay gameDaySoFar team answers =
        let addTeam gameDay =
            gameDay
            |> GameDay.withTeam team answers
            |> Result.mapError (fun _ -> team.Name.Value |> DuplicatedTeam)
        
        gameDaySoFar
        |> Result.bind addTeam
    
    teamsToParse
    |> Seq.map parse
    |> Result.combine 
    |> Result.bind (fun seq -> seq |> Seq.fold (fun acc (team, answers) -> updateGameDay acc team answers) gameDay)
    
    
    

let parseGameday (gameName : GameName) (document : HtmlDocument) = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"

    let parseTournamentInfo() =
        
        let titleParts = 
            document
            |> parseTitle
            |> String.replace "Таблица " ""
            |> String.splitByCharWithCount [| ' ' |] 2
        
        match titleParts with
        | [| season; league |] ->
            
            result {
                let! leagueName = league |> NoEmptyString.ofString |> expectMissingLeagueName
                let! seasonName = season |> NoEmptyString.ofString |> expectMissingSeasonName
                return
                    {
                        City = "Санкт-Петербург" |> NoEmptyString.ofConstString
                        League = leagueName
                        Season = seasonName
                    }
            }
        | [| _ |] -> Error MissingLeagueName
        | _ -> Error MissingSeasonName

    let parserOptions (sheetNode : HtmlNode) = 
        
        let optionsLineNode = 
            
            sheetNode
            |> HtmlNode.firstElement
            |> HtmlNode.elements

        let answerColumns = 
            optionsLineNode
            |> Seq.mapi (fun i node -> if node.DirectInnerText() |> isInt then Some i else None)
            |> Seq.choose id
            |> List.ofSeq

        {
            IdColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) number))
            NameColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) ""))
            SumColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) sum))
            AnswersColumns = answerColumns
        }

    let parseGameDay options sheetNode sheetId gameDay = 
        
        let parse node = 
            
            let innerTextOfNode n index = 
                n 
                |> HtmlNode.elements
                |> Seq.item index 
                |> HtmlNode.innerText

            let innerTextOfNode' = innerTextOfNode node
                
            result {
                let! team =
                    innerTextOfNode'
                    |> parseTeam options.IdColumn options.NameColumn 
                
                let answers = 
                
                    options.AnswersColumns
                    |> List.map (innerTextOfNode' >> ((=) rightAnswer) >> Answer.ofBool)
                    |> Answers.ofSeq
                    
                return team, answers
            }
                
        let filterBySheetId n = 
                    
            n 
            |> HtmlNode.elements 
            |> List.exists (fun c -> String.startsWith sheetId <| c.AttributeValue("id"))

        let teamsLines = 
            sheetNode
            |> HtmlNode.elements
            |> Seq.tail // may be remove?
            |> Seq.filter filterBySheetId
            |> Seq.exceptLast
        
        teamsLines
        |> createGameDay gameDay parse
    
    let gameDay = 

        result{
            let! sheetNode = findSheetNode document gameName
            let! sheetId = getSheetId document gameName
            let! tournament = parseTournamentInfo()

            let options = parserOptions sheetNode

            let emptyGameDay =
                
                options.AnswersColumns
                |> List.length
                |> PositiveNum.ofInt
                |> expectMissingAnswers
                |> Result.map (fun num -> {Tournament = tournament; Name = gameName; Answers = Map.empty; PackageSize = num})

            return! parseGameDay options sheetNode sheetId emptyGameDay
        }

    gameDay
    
let parse60SecondGameDay tournamentInfo gameName json =
    
    let parseTeam teamJson =
        result {
            let teamId = teamJson?team_id.AsInteger()
            let teamName = teamJson?title.AsString()
            let! team = createTeam teamId teamName
            
            let answers =
                teamJson?newmask.AsArray()
                |> Array.map (fun num -> num.AsDecimal() |> Answer.ofDecimal)
                |> Answers.ofSeq
            
            return team, answers
        } |> expectTeamParsingError
        
    match json with
    | JsonValue.Record props ->
        let results = props |> Array.find (fun p -> fst p = "results") |> snd
        let teamResults = results.AsString() |> JsonValue.Parse
    
        let questionsCount =
            let team = 
                seq {
                    for jsonTeam in teamResults do
                        yield jsonTeam 
                    } |> Seq.head
            team?newmask.AsArray()
            |> Array.length
            
        let emptyGameday =
            result {
                let! packageSize = PositiveNum.ofInt questionsCount
                return
                    {
                        Tournament = tournamentInfo
                        Name = gameName
                        Answers = Map.empty
                        PackageSize = packageSize 
                    }
            }
            |> expectMissingAnswers
            
        
        seq {
            for jsonTeam in teamResults do
                yield jsonTeam 
            }
        |> createGameDay emptyGameday parseTeam 
    | x -> x.AsString()  |> UnexpectedJson |> Error 
        
let parseTotalFromGoogleSpreadsheet document = 
    
    let parseLine options node = 
        
        let innerTextOfNode n index = 
            n 
            |> HtmlNode.elements
            |> Seq.item index 
            |> HtmlNode.innerText

        let innerTextOfNode' = innerTextOfNode node

        let tryParseDecimal s = 
            s 
            |> String.replace "," "."
            |> (fun s -> if s <> "" then s |> decimal |> Some else None)

        let filterResultNodes elements =
            elements
            |> Seq.mapi (fun index value -> (index, value))
            |> Seq.filter (fun (index, _) -> index <> options.FinalResultColumn)
            |> Seq.skip options.FirstResultColumn
            |> Seq.map (fun (_, value) -> value)
        
        let res = 
            node
            |> HtmlNode.elements
            |> filterResultNodes
            |> Seq.map (HtmlNode.innerText >> tryParseDecimal)
            |> Seq.choose id
            |> Seq.map Converter.pointFromDecimal
        
        innerTextOfNode'
        |> parseTeam options.TeamIdColumn options.TeamNameColumn
        |> Result.map (fun team -> (team, res))

    let seasonRating = 

        result{
            
            let! sheetNode =
                "60 сек"
                |> NoEmptyString.ofConstString
                |> findSheetNode document
                 
                
            let parserOptions = 
                
                let optionsLineNode = 
                    
                    sheetNode
                    |> HtmlNode.firstElement
                    |> HtmlNode.elements

                {
                    TeamIdColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "№"))
                    TeamNameColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Команда"))
                    FirstResultColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "сум")) |> (+) 2
                    FinalResultColumn = optionsLineNode |> Seq.length |> (+) -1
                }
                
            let linesWithTeams =
                sheetNode
                |> HtmlNode.elements
                |> Seq.skip 2
                |> (Seq.exceptLast >> Seq.exceptLast)
                
                
            let seasonTable data =
                data
                |> SeasonTable.ofSeq
                |> expectSeasonHasNotStartedError

            return!
                linesWithTeams
                |> Seq.map (parseLine parserOptions)
                |> Result.combine
                |> Result.bind seasonTable
        }
    seasonRating
    
let parseTotalFrom60SecSite document =
    
    let tableNodes =
        document
        |> HtmlDocument.body
        |> HtmlNode.descendants
        |> Seq.filter (HtmlNode.hasAttribute "id" "rate_table")
        // first one is about Первая лига, the second one is about Высшая лига 
        |> Seq.head
        
    let options = 
        
        let optionsLineNode = 
            tableNodes
            |> HtmlNode.elements
            |> List.head
            |> HtmlNode.elements
            |> List.head
            |> HtmlNode.elements
            
        {
            TeamIdColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Название"))
            TeamNameColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Название"))
            FirstResultColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Сумма")) |> (+) 1
            FinalResultColumn = optionsLineNode |> Seq.length |> (+) -1
        }   
        
    let parseLine options node =
        
        let innerTextOfNode n index = 
            n 
            |> HtmlNode.elements
            |> Seq.item index 
            |> HtmlNode.innerText

        let innerTextOfNode' = innerTextOfNode node

        let tryParseDecimal s = 
            s 
            |> String.replace "," "."
            |> (fun s -> if s <> "" then s |> decimal |> Some else None)

        let filterResultNodes elements =
            elements
            |> Seq.skip options.FirstResultColumn
        
        let res = 
            node
            |> HtmlNode.elements
            |> filterResultNodes
            |> Seq.map (HtmlNode.innerText >> tryParseDecimal)
            |> Seq.choose id
            |> Seq.map Converter.pointFromDecimal
            
        let team() =
            let teamId =
                let attrValue = 
                    node 
                    |> HtmlNode.elements
                    |> Seq.item options.TeamIdColumn
                    |> HtmlNode.elements
                    |> List.head
                    |> HtmlNode.attributes
                    |> List.find (fun a -> a.Name() = "href")
                    |> HtmlAttribute.value
                
                let str = 
                    attrValue
                    |> String.replace "/team/" ""
                    |> String.replace "/" ""
                int str
                
                
            let name = options.TeamNameColumn |> innerTextOfNode'
            createTeam teamId name
            |> expectTeamParsingError
        
        team() 
        |> Result.map (fun team -> (team, res))
        
    let total =
        let teamLines = 
            tableNodes
            |> HtmlNode.elements
            |> List.tail
            |> List.head
            |> HtmlNode.elements
            
        let seasonTable data =
            data
            |> SeasonTable.ofSeq
            |> expectSeasonHasNotStartedError
            
        teamLines
        |> Seq.map (parseLine options)
        |> Result.combine
        |> Result.bind seasonTable
    
    total