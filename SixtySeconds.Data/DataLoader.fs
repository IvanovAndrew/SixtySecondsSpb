module SixtySeconds.Data.DataLoader

open FSharp.Data

open SixtySeconds.Common.Errors
open SixtySeconds.Common.CommonTypes

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