module Utils

    
module StringUtils = 
    open System
    
    let splitByChar sep (str : string) = str.Split(sep)
    let splitByString (sep : string array) (str : string) = str.Split(sep, StringSplitOptions.RemoveEmptyEntries)

    let length (str : string) = str.Length

    let replace (oldValue : string) newValue (s : string) = s.Replace(oldValue, newValue)

    let containsLetter (str : string) = 
        str
        |> String.exists Char.IsLetter

    let startsWith subString (string : string) = string.StartsWith(subString)


type NoEmptyString = private NoEmptyString of string
    module NoEmptyString = 
        
        let ofString s = 
            if System.String.IsNullOrEmpty s 
            then invalidArg s "String must not be empty!" 
            else NoEmptyString s

        let tryOfString s = 
            if System.String.IsNullOrEmpty s 
            then None
            else Some <| NoEmptyString s

        let value (NoEmptyString s) = s

        let concat sep strings = 
            strings
            |> Seq.map value 
            |> String.concat sep
            |> ofString

type PositiveNum = private PositiveNum of int
module PositiveNum = 
    
    let ofInt i = 
        if i <= 0 then invalidArg "i" "Number must be positive!"
        else PositiveNum i
    
    let value (PositiveNum i) = i

    let createRange first step last = 
        [value first.. value step .. value last ]
        |> List.map ofInt 

    let createNaturalRange last = 
        
        let numOne = ofInt 1
        createRange numOne numOne last 