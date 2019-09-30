module Utils

    
module StringUtils = 
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
            
            if StringUtils.isEmpty str
            then invalidArg "str" "String must not be empty!" 
            else str |> NoEmptyString 

        static member value (NoEmptyString s) = s

        static member concat sep strings = 
            strings
            |> Seq.map NoEmptyString.value 
            |> String.concat sep
            |> NoEmptyString.ofString


module PositiveNum = 
    
    type PositiveNum = private PositiveNum of int
       
    let ofInt i = 
        if i <= 0 then invalidArg "i" "Number must be positive!"
        else PositiveNum i

    let value (PositiveNum i) = i

    let numOne = ofInt 1

    let previous (PositiveNum i) = ofInt <| i - 1
    let next (PositiveNum i) = ofInt <| i + 1
    
    let createRange first step last =
        
        seq {for i in value first .. value step .. value last -> i}
        |> List.ofSeq
        |> List.map ofInt

    let createNaturalRange last = createRange numOne numOne last 