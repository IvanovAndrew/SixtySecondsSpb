module Parser

open FSharp.Data
open System
open Utils
open Domain

type ParserOption = 
        {
            IdColumn : int 
            NameColumn : int 
            SumColumn : int
            AnswersColumns : int list
        }


let parse sheetName url = 
    
    let number = "№"
    let name = ""
    //let points = "баллы"
    let sum = "сум"
    let firstAnswer = "1"
    let rightAnswer = "1"
            
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

    let sheetId = 
        
        let elem = 
            document.Elements()
            |> List.ofSeq
            |> List.head
        
        let childs = 
            elem.Descendants() 
            |> List.ofSeq

        let c = 
            childs
            |> List.filter (fun c -> c.InnerText().Contains(sheetName))
            |> List.head
            
        let node = 
            c.Descendants()
            |> Seq.filter (fun n -> n.InnerText() = sheetName)
            |> Seq.filter (fun n -> n.Attributes() |> List.exists (fun a -> a.Name() = "id"))
            |> Seq.head
            
        node.Attribute("id").Value()
        |> StringUtils.replace "sheet-button-" ""

    let elements (node : HtmlNode) = node.Elements()

    let firstElement node = 
        node 
        |> elements 
        |> Seq.head

    let sheetNode = 
        
        let elem = 
            document.Elements()
            |> Seq.head

        elem.Descendants()
        |> Seq.filter (fun n -> n.HasAttribute("id", sheetId))
        |> Seq.head
        |> firstElement
        |> firstElement
        |> elements
        |> List.ofSeq
        |> List.tail
        |> List.head

    let parseOptions() = 
        
        let optionsLineNode = 
            
            sheetNode
            |> firstElement
            |> elements

        let isInt s = 
            match Int32.TryParse(s) with 
            | (true, _) -> true
            | _ -> false

        let q = 
            optionsLineNode
            |> Seq.mapi (fun i node -> if node.DirectInnerText() |> isInt then Some i else None)
            |> Seq.choose (fun i -> i)
            |> List.ofSeq

        {
            IdColumn = optionsLineNode |> Seq.findIndex (fun n -> n.InnerText() = number)
            NameColumn = optionsLineNode |> Seq.findIndex (fun n -> n.InnerText() = "")
            SumColumn = optionsLineNode |> Seq.findIndex (fun n -> n.InnerText() = sum)
            AnswersColumns = q
        }

    let parserOptions = parseOptions() 

    let parseGameDay() = 
        
        let parse node = 
            
            let innerTextOfElement index n = 
                
                let t = 
                            
                    n 
                    |> elements
                    |> Seq.item index 
                
                t.InnerText()

            

            let team = 
                {
                    ID = node |> innerTextOfElement parserOptions.IdColumn |> int |> PositiveNum.ofInt 
                    Name = node |> innerTextOfElement parserOptions.NameColumn |> NoEmptyString.ofString
                }

            let answers = 
                
                parserOptions.AnswersColumns
                |> List.map (fun column -> node |> innerTextOfElement column |> (=) rightAnswer) 
                |> List.map Answer.ofBool
                |> Array.ofList
                |> Answers.ofArray

            team, answers

                
        let filterBySheetId n = 
                    
            n 
            |> elements 
            |> List.exists (fun c -> StringUtils.startsWith sheetId <| c.AttributeValue("id"))

                
        let exceptLast list = 
            list 
            |> List.rev
            |> List.tail
            |> List.rev

        sheetNode
        |> elements
        |> List.ofSeq
        |> List.tail
        |> List.filter filterBySheetId
        |> exceptLast
        |> List.map parse
        |> Map.ofList
    
    let map = parseGameDay()

    let teams = map |> Map.toSeq |> Seq.map fst
    let questionsCount = map |> Map.find (teams |> Seq.head) |> Answers.count
    let gameDay = {Day = DateTime.Now; Answers = map; QuestionsCount = questionsCount}

    teams, gameDay