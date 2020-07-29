module Utils

open System

// StringBuilder snippet from http://www.fssnip.net/by/title/Building-Strings
let Out x = x.ToString()

type Object with

    member this.Out : string =
        this |> Out

// Too lazy to use foo.Append(bar).

let (++) (left : System.Text.StringBuilder) (right : 't) : System.Text.StringBuilder =
    left.Append right

// Too lazy to use foo.Append(bar) |> ignore

let (+=) (left : System.Text.StringBuilder) (right : 't) : unit =
    left ++ right |> ignore
    
    

    

