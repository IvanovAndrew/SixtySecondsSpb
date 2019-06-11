module Parser

open System
open System.Text.RegularExpressions

open FSharp.Data

open Utils
open Domain

open FSharp.Data

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
    let descendants (node : HtmlNode) = node.Descendants()
    let attributes (node : HtmlNode) = node.Attributes()
    let attribute name (node : HtmlNode) = node.Attribute(name)

module Attribute = 
    
    let name (attribute : HtmlAttribute) = attribute.Name()
    let value (attribute : HtmlAttribute) = attribute.Value()


let parse sheetName url = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"

    

    let firstElement = HtmlNode.elements >> Seq.head
            
    let document = 

        let getRequest uri = 
            let response = Http.Request(uri, silentHttpErrors = true)
            match response.Body with 
            | HttpResponseBody.Text text -> text
            | x -> failwithf "%A" x
                
        url
        |> getRequest
        |> StringUtils.replace "<style>@import url(https://fonts.googleapis.com/css?kit=o--8Et3j0xElSo4Jk-6CSN_pgL91BiSHK8etQbSopkk);</style>" "" 
        |> HtmlDocument.Parse 

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

    let sheetId = 
        
        let childs = 
            document.Elements()
            |> List.ofSeq
            |> List.head
            |> HtmlNode.descendants

        let node = 
            childs
            |> Seq.filter (HtmlNode.innerText >> StringUtils.containsString sheetName)
            |> Seq.head
            |> HtmlNode.descendants
            |> Seq.filter (HtmlNode.innerText >> ((=) sheetName))
            |> Seq.filter (HtmlNode.attributes >> List.exists (Attribute.name >> ((=) "id")))
            |> Seq.head
            
        node
        |> HtmlNode.attribute "id"
        |> Attribute.value
        |> StringUtils.replace "sheet-button-" ""

    let sheetNode = 
        
        let elem = 
            document.Elements()
            |> Seq.head

        elem
        |> HtmlNode.descendants
        |> Seq.filter (fun n -> n.HasAttribute("id", sheetId))
        |> Seq.head
        |> firstElement
        |> firstElement
        |> HtmlNode.elements
        |> Seq.tail
        |> Seq.head

    let parserOptions = 
        
        let optionsLineNode = 
            
            sheetNode
            |> firstElement
            |> HtmlNode.elements

        let isInt s = 
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

    let parseGameDay() = 
        
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
                    
            n 
            |> HtmlNode.elements 
            |> List.exists (fun c -> StringUtils.startsWith sheetId <| c.AttributeValue("id"))

                
        let exceptLast s = 
            s
            |> Seq.rev
            |> Seq.tail
            |> Seq.rev

        sheetNode
        |> HtmlNode.elements
        |> Seq.tail
        |> Seq.filter filterBySheetId
        |> exceptLast
        |> Seq.map parse
        |> Map.ofSeq
    
    let gameDate = 
        let m = Regex.Match(sheetName, "(\d{1,2})\.(\d{1,2})")
        let day = m.Groups.[1].Value |> int
        let month = m.Groups.[2].Value |> int

        new DateTime(year, month, day)

    let map = parseGameDay()

    let teams = map |> Map.toSeq |> Seq.map fst
    let questionsCount = map |> Map.find (teams |> Seq.head) |> Answers.count
    let gameDay = {Day = gameDate; Answers = map; QuestionsCount = questionsCount}

    teams, gameDay