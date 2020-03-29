namespace Answers

open Domain
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open FsUnit
open System

open TestUtils
open Utils


module ``Find strike specification`` =
    
    open Domain
    open TestUtils.FsCheckUtils
    
    let reverseAnswers answers =
        answers
        |> Answers.toList
        |> List.rev
        |> Answers.ofAnswersList
        
    let invertAnswers answers =
        
        let invertAnswer a =
            {a with Answer = match a.Answer with Right -> Wrong | Wrong -> Right}
        
        answers
        |> Answers.toList
        |> List.map invertAnswer
        |> Answers.ofAnswersList
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<AnswersStrikeGenerator>|])>]
    let ``Answers strike should be same as strike of reversed answers`` (strike, answers) =
        
        let answersStrikes =
            answers
            |> Answers.findStrike strike
        
        let reversedStrikes =
            answers
            |> reverseAnswers
            |> Answers.findStrike strike
        
        answersStrikes = reversedStrikes
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<AnswersStrikeGenerator>|])>]
    let ``Answers strike is same as opposite strike of inverted answers``(strike, answers) =
        
        let answersStrikes =
            answers
            |> Answers.findStrike strike
        
        let invertedOppositeStrike =
            answers
            |> invertAnswers
            |> Answers.findStrike (match strike with Best -> Worst | Worst -> Best)
        
        answersStrikes = invertedOppositeStrike
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<RightAnswersGenerator>|])>]
    let ``If all answers are right then best strike will be equal to answers count`` answers =
        
        let strike =
            answers
            |> Answers.findStrike Best
            
        let count = answers |> Answers.count
        
        strike = count