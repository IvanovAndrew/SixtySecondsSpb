module Utils

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

let toResultOfSeq acc result = 
    
    acc 
    |> Result.bind (fun items -> match result with Ok item -> Ok ([item] |> Seq.append items) | Error e -> Error e)
    
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

        static member value (NoEmptyString s) = s

module PositiveNum = 
    
    type PositiveNum = private PositiveNum of int
       
    let ofInt i = 
        if i <= 0 then Error "Number must be positive!"
        else Ok <| PositiveNum i

    let ofString (str : string) = 
        
        match System.Int32.TryParse(str) with 
        | true, num -> ofInt num
        | _ -> Error "Number expected"

    let value (PositiveNum i) = i

    let numOne = PositiveNum(1)

    let previous (PositiveNum i) = i - 1 |> ofInt

    let next (PositiveNum i) = PositiveNum(i + 1)
        
    let createRange first step last =
        
        seq {for i in value first .. value step .. value last -> i}
        |> List.ofSeq
        |> List.map PositiveNum

    let createNaturalRange last = createRange numOne numOne last 

type Url = private Url of string
module Url = 

    let create (url : string) = 
        if url.StartsWith(@"http://") || url.StartsWith(@"https://") 
        then Ok <| Url url
        else Error "Input must be correct url"

    let value (Url url) = url