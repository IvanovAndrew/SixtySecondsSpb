module Parser

open System
open System.Text.RegularExpressions

open FSharp.Data

open Utils
open Domain

type ParserOption = 
    {
        IdColumn : int 
        NameColumn : int 
        SumColumn : int
        AnswersColumns : int list
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

let private loadDocument url = 
    
    let getRequest uri = 
        let response = Http.Request(uri, silentHttpErrors = true)
        match response.Body with 
        | HttpResponseBody.Text text -> text
        | x -> failwithf "%A" x
                
    url
    |> getRequest
    |> StringUtils.replace "<style>@import url(https://fonts.googleapis.com/css?kit=o--8Et3j0xElSo4Jk-6CSN_pgL91BiSHK8etQbSopkk);</style>" "" 
    |> HtmlDocument.Parse 

let private getSheetId (document : HtmlDocument) sheetName = 
    
    let childs = 
        document.Elements()
        |> List.ofSeq
        |> List.head
        |> HtmlNode.descendants

    let nodeOption = 
        childs
        |> Seq.filter (HtmlNode.innerText >> StringUtils.containsSubstring sheetName)
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
        |> StringUtils.replace "sheet-button-" ""
    | None -> invalidArg "sheetName" <| sprintf "Sheet %s not found" sheetName

let private findSheetNode url sheetName = 
    
    let document = loadDocument url

    let sheetId = getSheetId document sheetName

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

    sheetNode



let parse sheetName url = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"

    let document = loadDocument url

    let year = 
        let title = 
            
            document.Elements()
            |> List.ofSeq
            |> List.head
            |> HtmlNode.descendants
            |> Seq.item 2
            |> HtmlNode.innerText

        let m = Regex.Match(title, "\d{4}|\d{4}\\\d{4}")
        m.Value |> int

    let sheetNode = findSheetNode url sheetName

    let parserOptions = 
        
        let optionsLineNode = 
            
            sheetNode
            |> HtmlNode.firstElement
            |> HtmlNode.elements

        let isInt (s : string) = 
            match Int32.TryParse(s) with 
            | (true, v) -> true
            | _ -> false

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

    let parseGameDay gameDay = 
        
        let parse node = 
            
            let innerTextOfNode n index = 
                n 
                |> HtmlNode.elements
                |> Seq.item index 
                |> HtmlNode.innerText

            let innerTextOfNode' = innerTextOfNode node

            let team = 
                {
                    ID = parserOptions.IdColumn |> innerTextOfNode' |> int |> PositiveNum.ofInt 
                    Name = parserOptions.NameColumn |> innerTextOfNode' |> NoEmptyString.ofString
                }

            let answers = 
                
                parserOptions.AnswersColumns
                |> List.map (innerTextOfNode' >> ((=) rightAnswer) >> Answer.ofBool)
                |> Answers.ofSeq

            team, answers

                
        let filterBySheetId n = 
                    
            let sheetId = getSheetId document sheetName

            n 
            |> HtmlNode.elements 
            |> List.exists (fun c -> StringUtils.startsWith sheetId <| c.AttributeValue("id"))

                
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
        
        teamsLines
        |> Seq.map parse
        |> Seq.fold (fun g (team, answers) -> GameDay.withTeam team answers g) gameDay
    
    let gameDay =
        
        let gameDate = 
            let m = Regex.Match(sheetName, "(\d{1,2})\.(\d{1,2})")
            let day = m.Groups.[1].Value |> int
            let month = m.Groups.[2].Value |> int

            new DateTime(year, month, day)
        
        parseGameDay {Day = gameDate; Answers = Map.empty; QuestionsCount = PositiveNum.ofInt 36}
    gameDay

let parseTotal url = 
    
    let sheetNode = findSheetNode url "60 сек"

    let teamId, teamName, firstResult = 
        
        let optionsLineNode = 
            
            sheetNode
            |> HtmlNode.firstElement
            |> HtmlNode.elements

        optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "№")),
        optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "Команда")),
        optionsLineNode |> Seq.findIndex (HtmlNode.innerText >> ((=) "сум")) |> (+) 2
    
    let parseLine node = 
        
        let innerTextOfNode n index = 
                n 
                |> HtmlNode.elements
                |> Seq.item index 
                |> HtmlNode.innerText

        let innerTextOfNode' = innerTextOfNode node

        let team = 
            {
                ID = teamId |> innerTextOfNode' |> int |> PositiveNum.ofInt
                Name = teamName |> innerTextOfNode' |> NoEmptyString.ofString
            }

        let tryParseDecimal s = 
            s 
            |> StringUtils.replace "," "."
            |> (fun s -> if s <> "" then s |> decimal |> Some else None)

        let res = 
            node
            |> HtmlNode.elements
            |> Seq.skip firstResult
            |> Seq.map (HtmlNode.innerText >> tryParseDecimal)
            |> Seq.choose id
        
        team, res

    sheetNode
    |> HtmlNode.elements
    |> (Seq.rev >> Seq.skip 2 >> Seq.rev) // except last
    |> Seq.skip 2
    |> Seq.map parseLine
    |> SeasonTable.ofSeq