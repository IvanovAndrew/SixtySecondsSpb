module Parser

open System
open System.Text.RegularExpressions

open FSharp.Data

open Utils
open Domain

let asyncLoadDocument url = 
    
    async {
        
        let! response = Http.AsyncRequest(url, silentHttpErrors = true)

        let result = 
            if response.StatusCode <> 200 then Error "Page not found"
            else 
                match response.Body with 
                | HttpResponseBody.Text text -> 
                    text
                    |> String.replace "<style>@import url(https://fonts.googleapis.com/css?kit=o--8Et3j0xElSo4Jk-6CSN_pgL91BiSHK8etQbSopkk);</style>" "" 
                    |> HtmlDocument.Parse 
                    |> Ok
                | _ -> Error "Unexpected response body"

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
        |> Seq.filter (HtmlNode.innerText >> String.containsSubstring sheetName)
        |> Seq.tryHead
        
            
    match nodeOption with 
    | Some node -> 
        node
        |> HtmlNode.descendants
        |> Seq.filter (HtmlNode.innerText >> ((=) sheetName))
        |> Seq.filter (HtmlNode.attributes >> List.exists (Attribute.name >> ((=) "id")))
        |> Seq.head
        |> HtmlNode.attribute "id"
        |> Attribute.value
        |> String.replace "sheet-button-" ""
        |> Ok
    | None -> Error <| sprintf "Sheet %s not found" sheetName

let private findSheetNode document sheetName = 
    
    match getSheetId document sheetName with 
    | Ok sheetId -> 

        let sheetNode = 
        
            let elem = 
                document.Elements()
                |> Seq.head

            elem
            |> HtmlNode.descendants
            |> Seq.filter (fun n -> n.HasAttribute("id", sheetId))
            |> Seq.head
            |> HtmlNode.firstElement
            |> HtmlNode.firstElement
            |> HtmlNode.elements
            |> Seq.tail
            |> Seq.head

        Ok sheetNode
    | Error e -> Error e



let parse sheetName (document : HtmlDocument) = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"

    let getYear() = 
        let title = 
            
            document.Elements()
            |> Seq.head
            |> HtmlNode.descendants
            |> Seq.item 2
            |> HtmlNode.innerText

        let yearString = Regex.Match(title, "\d{4}|\d{4}\\\d{4}")
        yearString.Value |> int

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

            let team = 
                
                result{
                    let! teamId = options.IdColumn |> innerTextOfNode' |> PositiveNum.ofString
                    let! name = options.NameColumn |> innerTextOfNode' |> NoEmptyString.ofString

                    return {
                        ID = teamId
                        Name = name
                    }
                }
                
            team
            |> Result.map (fun t -> (t, answers))

                
        let filterBySheetId n = 
                    
            n 
            |> HtmlNode.elements 
            |> List.exists (fun c -> String.startsWith sheetId <| c.AttributeValue("id"))

                
        let exceptLast s = 
            s
            |> Seq.rev
            |> Seq.tail
            |> Seq.rev
        
        let teamsLines = 
            sheetNode
            |> HtmlNode.elements
            |> Seq.tail
            |> Seq.filter filterBySheetId
            |> exceptLast
        
        let createGameDay gameDaySoFar team answers = 
            
            match gameDaySoFar with 
            | Ok gameDay -> 
                gameDay |> GameDay.withTeam team answers
            | Error e -> Error e

        teamsLines
        |> Seq.map parse
        |> Seq.fold toResultOfSeq (Ok Seq.empty)
        |> Result.bind (fun seq -> seq |> Seq.fold (fun acc (team, answers) -> createGameDay acc team answers) gameDay)
    
    let gameDate() = 
        let dateString = Regex.Match(sheetName, "(\d{1,2})\.(\d{1,2})")
        let day = dateString.Groups.[1].Value |> int
        let month = dateString.Groups.[2].Value |> int

        let year = getYear()

        new DateTime(year, month, day)

    

    let gameDay = 

        result{
            let! sheetNode = findSheetNode document sheetName
            let! sheetId = getSheetId document sheetName
            let gameDate = gameDate()

            let options = parserOptions sheetNode

            let emptyGameDay = 
                36 
                |> PositiveNum.ofInt
                |> Result.map (fun num -> {Day = gameDate; Answers = Map.empty; PackageSize = num})

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
        
        let team = 
            
            result {
                let! teamId = options.TeamIdColumn |> innerTextOfNode' |> PositiveNum.ofString
                let! name = options.TeamNameColumn |> innerTextOfNode' |> NoEmptyString.ofString

                return {ID = teamId; Name = name}
            }

        team
        |> Result.map (fun team -> (team, res))

    let seasonRating = 

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

            

            let okList = Ok Seq.empty

            return!
                sheetNode
                |> HtmlNode.elements
                |> (Seq.rev >> Seq.skip 2 >> Seq.rev) // except last
                |> Seq.skip 2
                |> Seq.map (parseLine parserOptions)
                |> Seq.fold toResultOfSeq okList
                |> Result.bind SeasonTable.ofSeq
        }
    seasonRating