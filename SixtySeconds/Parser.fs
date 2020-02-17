module Parser

open FSharp.Data

open Utils
open Domain

type ParsingError =
    | MissingSheetName
    | MissingTournamentName
    | MissingAnswersCount
    | TeamParsingError of string
    | SheetNotFound of string
    | DuplicatedTeam of Team
    | SeasonHasNotStarted
    
    
type WebRequestError = 
    | PageNotFound of string
    | UnexpectedResponseBody 
    
type SixtySecondsError =
    | ParsingError of ParsingError
    | WebRequestError of WebRequestError
    
    
let errorToString = function
    | ParsingError error ->
        match error with 
        | MissingSheetName -> "Missing sheet name"
        | MissingTournamentName -> "Missing tournament name"
        | MissingAnswersCount -> "Missing answers count"
        | TeamParsingError err -> sprintf "Can not parse team. %s" err
        | SheetNotFound sheetName -> sprintf "Sheet %s not found" sheetName
        | SeasonHasNotStarted -> "Season hasn't started yet"
        | DuplicatedTeam team -> sprintf "Team %s is already added " <| NoEmptyString.value team.Name
    | WebRequestError error ->
        match error with
        | PageNotFound url -> sprintf "Page %s not found" url
        | UnexpectedResponseBody -> "Unexpected response body"

let asyncLoadDocument url = 
    
    async {
        let urlString = Url.value url
        let! response = Http.AsyncRequest(urlString, silentHttpErrors = true)

        let result = 
            if response.StatusCode <> 200 then urlString |> PageNotFound |> Error
            else
                match response.Body with 
                | HttpResponseBody.Text text -> 
                    text
                    |> String.replace "<style>@import url(https://fonts.googleapis.com/css?kit=o--8Et3j0xElSo4Jk-6CSN_pgL91BiSHK8etQbSopkk);</style>" "" 
                    |> HtmlDocument.Parse 
                    |> Ok
                | _ -> UnexpectedResponseBody |> Error 

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
    }

module HtmlNode = 
    
    let innerText (node : HtmlNode) = node.InnerText()
    let elements (node : HtmlNode) = node.Elements()
    let firstElement (node : HtmlNode) = node |> elements |> Seq.head
    let descendants (node : HtmlNode) = node.Descendants()
    let attributes (node : HtmlNode) = node.Attributes()
    let attribute name (node : HtmlNode) = node.Attribute(name)
    let hasAttribute attribute value (node : HtmlNode) = node.HasAttribute(attribute, value)
    
module HtmlDocument =
    
    let elements (document : HtmlDocument) = document.Elements()

module Attribute = 
    
    let name (attribute : HtmlAttribute) = attribute.Name()
    let value (attribute : HtmlAttribute) = attribute.Value()

let private getSheetId (document : HtmlDocument) sheetName = 
    
    let children = 
        document.Elements()
        |> List.ofSeq
        |> List.head
        |> HtmlNode.descendants

    let nodeOption = 
        children
        |> Seq.tryFind (HtmlNode.innerText >> String.containsSubstring sheetName)
        
            
    match nodeOption with 
    | Some node -> 
        node
        |> HtmlNode.descendants
        |> Seq.filter (HtmlNode.innerText >> ((=) sheetName))
        |> Seq.find (HtmlNode.attributes >> List.exists (Attribute.name >> ((=) "id")))
        |> HtmlNode.attribute "id"
        |> Attribute.value
        |> String.replace "sheet-button-" ""
        |> Ok
    | None ->
        sheetName
        |> SheetNotFound 
        |> Error
        
let private parseTeam idColumn nameColumn innerText =
            
    let teamId = idColumn |> innerText |> int
    let name = nameColumn |> innerText
    createTeam teamId name
    |> Result.mapError TeamParsingError

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

let parse sheetName (document : HtmlDocument) = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"
    
    let getSheetTitle() =
        
        document
        |> HtmlDocument.elements
        |> Seq.head
        |> HtmlNode.descendants
        |> Seq.item 2
        |> HtmlNode.innerText

    let getTournamentName() =
        
        getSheetTitle()
        |> String.replace "Таблица " ""
        |> NoEmptyString.ofString
        |> Result.mapError (fun _ -> MissingTournamentName)

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

            let answers = 
                
                options.AnswersColumns
                |> List.map (innerTextOfNode' >> ((=) rightAnswer) >> Answer.ofBool)
                |> Answers.ofSeq
                
            innerTextOfNode'
            |> parseTeam options.IdColumn options.NameColumn
            |> Result.map (fun t -> (t, answers))

                
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
        
        let createGameDay gameDaySoFar team answers = 
            
            let addTeam gameDay =
                gameDay
                |> GameDay.withTeam team answers
                |> Result.mapError (fun _ -> DuplicatedTeam team)
            
            gameDaySoFar
            |> Result.bind addTeam

        teamsLines
        |> Seq.map parse
        |> Result.OfSeq (Ok Seq.empty)
        |> Result.bind (fun seq -> seq |> Seq.fold (fun acc (team, answers) -> createGameDay acc team answers) gameDay)
    
    let gameDay = 

        result{
            let! gameName = sheetName
                            |> NoEmptyString.ofString
                            |> Result.mapError (fun _ -> MissingSheetName)
            
            let! sheetNode = findSheetNode document sheetName
            let! sheetId = getSheetId document sheetName
            let! tournament = getTournamentName()
            

            let options = parserOptions sheetNode

            let emptyGameDay =
                options.AnswersColumns
                |> List.length
                |> PositiveNum.ofInt
                |> Result.mapError (fun _ -> MissingAnswersCount)
                |> Result.map (fun num -> {Tournament = tournament; Name = gameName; Answers = Map.empty; PackageSize = num})

            return! parseGameDay options sheetNode sheetId emptyGameDay
        }

    gameDay


let parseTotal document = 
    
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

        let res = 
            node
            |> HtmlNode.elements
            |> Seq.skip options.FirstResultColumn
            |> Seq.map (HtmlNode.innerText >> tryParseDecimal)
            |> Seq.choose id
            |> Seq.map Converter.pointFromDecimal
        
        innerTextOfNode'
        |> parseTeam options.TeamIdColumn options.TeamNameColumn
        |> Result.map (fun team -> (team, res))

    let seasonRating : Result<_, ParsingError> = 

        result{
            
            let! sheetNode = findSheetNode document "60 сек"
            let parserOptions = 
                
                let optionsLineNode = 
                    
                    sheetNode
                    |> HtmlNode.firstElement
                    |> HtmlNode.elements

                {
                    TeamIdColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "№"))
                    TeamNameColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Команда"))
                    FirstResultColumn = optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "сум")) |> (+) 2
                }
                
            let linesWithTeams =
                sheetNode
                |> HtmlNode.elements
                |> Seq.skip 2
                |> (Seq.exceptLast >> Seq.exceptLast)
                
                
            let seasonTable data =
                data
                |> SeasonTable.ofSeq
                |> Result.mapError (fun _ -> SeasonHasNotStarted)

            return!
                linesWithTeams
                |> Seq.map (parseLine parserOptions)
                |> Result.OfSeq (Ok Seq.empty)
                |> Result.bind seasonTable
        }
    seasonRating