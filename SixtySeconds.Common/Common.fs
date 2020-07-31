module SixtySeconds.Common.CommonTypes

open System
open System.Text

let (|SeqEmpty|SeqOneItem|SeqMore|) (xs: 'a seq) = //'
    if Seq.isEmpty xs then SeqEmpty
    else
        let tail = Seq.skip 1 xs
        if Seq.isEmpty tail then SeqOneItem(Seq.head xs)
        else SeqMore(Seq.head xs, tail)

let isInt (s : string) = 
    match System.Int32.TryParse(s) with 
    | (true, _) -> true
    | _ -> false

type ResultBuilder() =
    
    let ofOption error = function Some s -> Ok s | None -> Error error
    
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: Result<_, _>) = m

    member __.Bind(m, f) = Result.bind f m
    member __.Bind((m, error): (Option<'T> * 'E), f) = m |> ofOption error |> Result.bind f

    member __.Zero() = None

    member __.Combine(m, f) = Result.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.While(guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

let result = new ResultBuilder()

module Result =
    let ofOption errorData opt =
        match opt with
        | Some value -> Ok value
        | None -> Error errorData
        
    let toOption result =
        match result with
        | Ok value -> Some value
        | Error _ -> None
        
    let valueOrException = function
        | Ok v -> v
        | Error e -> failwith <| sprintf "%A" e

    let combine results =
        
        let rec loop acc results =
            
            if Seq.isEmpty results then acc
            else
                let head, tail = Seq.head results, Seq.tail results
                
                match head with
                | Error e -> Error e
                | Ok ok -> 
                    let newAcc = acc |> Result.map (fun oks -> ok |> Seq.singleton |> Seq.append oks)
                    loop newAcc tail
        results
        |> loop (Ok Seq.empty)
    
module String  = 
    open System
    
    let isEmpty (s : string) =  System.String.IsNullOrEmpty s || System.String.IsNullOrWhiteSpace s
    
    let splitByChar sep (str : string) = str.Split(sep)
    let splitByString (sep : string array) (str : string) = str.Split(sep, StringSplitOptions.RemoveEmptyEntries)

    let length (str : string) = str.Length

    let replace (oldValue : string) newValue (s : string) = s.Replace(oldValue, newValue)

    let containsSubstring (substring : string) (string : string) = string.Contains(substring)

    let startsWith (subString : string) (string : string) = string.StartsWith(subString)
    
        

    

type NoEmptyString = private NoEmptyString of string
    with
        static member ofString str =
            
            if String.isEmpty str
            then Error "String must not be empty!" 
            else str |> NoEmptyString |> Ok

        member this.Value = match this with NoEmptyString s -> s

type PositiveNum = private PositiveNum of int
    
    with
        static member ofInt i = 
            if i <= 0 then Error "Number must be positive!"
            else Ok <| PositiveNum i

        static member ofString (str : string) = 
            
            match System.Int32.TryParse(str) with 
            | true, num -> PositiveNum.ofInt num
            | _ -> Error "Number expected"

        member this.Value = match this with PositiveNum num -> num

        static member value (PositiveNum num) = num
        static member numOne = PositiveNum(1)
        

        static member add (PositiveNum one) (PositiveNum two) = PositiveNum (one + two)
        static member next = PositiveNum.add PositiveNum.numOne 
            
        static member createRange (first : PositiveNum) (step : PositiveNum) (last : PositiveNum) =
            
            seq {for i in first.Value .. step.Value .. last.Value -> i}
            |> List.ofSeq
            |> List.map PositiveNum

        static member createNaturalRange last = PositiveNum.createRange PositiveNum.numOne PositiveNum.numOne last 

type Url = private Url of string
module Url = 

    let create (url : string) = 
        if url.StartsWith(@"http://") || url.StartsWith(@"https://") 
        then Ok <| Url url
        else Error "Input must be correct url"

    let value (Url url) = url
    

let (|Sec60Season|Sec60Game|Google|Unknown|) site =
        
    match site |> Url.value with
    | x when x |> String.containsSubstring "https://60sec.online/season/" -> Sec60Season  
    | x when x |> String.containsSubstring "https://60sec.online/game/" -> Sec60Game
    | x when x |> String.containsSubstring "https://docs.google.com/spreadsheets/" -> Google
    | _ -> Unknown
    
module Seq =
    let exceptLast seq =
        
        let rec imp acc s =
        
            match s with
            // TODO maybe should throw exception?
            | SeqEmpty -> Seq.empty
            | SeqOneItem _ -> acc
            | SeqMore (head, tail) ->
                let newAcc =
                    head
                    |> Seq.singleton
                    |> Seq.append acc   
                imp newAcc tail
            
        imp Seq.empty seq
    
let bindAsync f a =
        async {
            let! a = a
            return! f a
        }