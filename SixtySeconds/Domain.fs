module Domain

open System
open System
open System.Numerics
open System.Text
open Utils
open PositiveNum


type Team = 
    {
        ID : PositiveNum
        Name : NoEmptyString
    }
    
let createTeam id name =
    result {
        let! positiveId = PositiveNum.ofInt id
        let! noEmptyName = NoEmptyString.ofString name
        
        return {ID = positiveId; Name = noEmptyName;}
    }

type Place = 
    {
        From : PositiveNum
        To : PositiveNum
    }

type Answer = 
    | Right 
    | Wrong
    
[<Measure>]
type RightAnswer

[<Measure>]
type Point

module Converter =
    
    let rightAnswerFromInt num = num * 1<RightAnswer>
    let pointFromDecimal num = num * 1m<Point>
    let toInt (num : int<_>) = int num
    let toDecimal (num : decimal<_>) = decimal num

module Answer =
    
    let ofBool isRight = if isRight then Right else Wrong
    let isRight answer = answer = Answer.Right
    let toRightAnswer answer = if isRight answer then 1<RightAnswer> else 0<RightAnswer> 
    
type Answers = private Answers of Answer array    
module Answers = 
    
    let ofArray arr = Answers arr
    let ofBoolArray arr = arr |> Array.map Answer.ofBool |> ofArray
    let ofSeq seq = seq |> Array.ofSeq |> ofArray
        
    let getAnswer questionNumber (Answers a) = 
        
        let index = 
            questionNumber
            |> PositiveNum.value 
            |> ((+) -1)
        
        if index >= Array.length a then
            let message = sprintf "There are only %d questions on the game" <| Array.length a
            invalidArg "questionNumber" message
        
        a.[index]

    let count (Answers a) = a |> Array.length |> PositiveNum.ofInt

type GameDay = 
    {
        Tournament : NoEmptyString
        Name : NoEmptyString
        Answers : Map<Team, Answers>
        PackageSize : PositiveNum
    }

module GameDay = 
        
    let withTeam team answers gameDay =
        
        let checkQuestionsCount a = 
            
            answers 
            |> Answers.count
            |> Result.bind(fun count -> if count = gameDay.PackageSize then Ok() else Error "Questions count mismatching")

        let checkIfTeamAdded t = 
            
            let teamAdded = 
                gameDay.Answers
                |> Map.containsKey t

            if teamAdded then sprintf "Team %s is already added" <| NoEmptyString.value t.Name |> Error 
            else Ok()

        result{
            
            let! _ = checkQuestionsCount answers
                
            let! _ = checkIfTeamAdded team
            
            let newAnswers = gameDay.Answers |> Map.add team answers
            return {gameDay with Answers = newAnswers}
        }
        
        
    let teams gameDay = 
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map fst
        
    /// Did team A answer question Q
    let getAnswer gameDay team questionNumber = 
            
        gameDay.Answers 
        |> Map.find team
        |> Answers.getAnswer questionNumber

    /// How many correct answers did the team give as of question Q
    let totalAnswered gameDay questionNumber team  = 
        
        let getAnswer' = getAnswer gameDay team
        
        questionNumber
        |> PositiveNum.createNaturalRange 
        |> Seq.map getAnswer'
        |> Seq.sumBy Answer.toRightAnswer

    /// Отставание команды A от лидера по состоянию на вопрос Q
    let getDistanceFromTheFirstPlace gameDay team questionNumber = 
            
        let totalAnswered' = totalAnswered gameDay questionNumber
        let teamAnsweredOn = totalAnswered' team 

        let leaderAnsweredOn = 
            gameDay
            |> teams
            |> Seq.map totalAnswered'
            |> Seq.max
        
        teamAnsweredOn - leaderAnsweredOn

    
    /// Team position after question Q
    let getPlaceAfterQuestion gameDay team questionNumber = 
        
        let totalAnswered' = totalAnswered gameDay questionNumber 
        let threshold = totalAnswered' team 

        let processTeam (placeUp, placeDown) otherTeam = 
            
            if otherTeam = team then placeUp, placeDown
            else 
                let answered = totalAnswered' otherTeam
                
                if answered > threshold then placeUp |> PositiveNum.next, placeDown |> PositiveNum.next
                elif answered = threshold then placeUp, placeDown |> PositiveNum.next
                else placeUp, placeDown


        let placeUp, placeDown = 
            gameDay
            |> teams
            |> Seq.fold processTeam (PositiveNum.numOne, PositiveNum.numOne)
            
        {
            From = placeUp
            To = placeDown
        }
        
    let getPlace gameDay team =
        getPlaceAfterQuestion gameDay team gameDay.PackageSize

    /// Number of teams that correctly answered on question 
    let rightAnswersOnQuestion gameDay question = 
        gameDay
        |> teams
        |> Seq.map (fun t -> getAnswer gameDay t question)
        |> Seq.sumBy Answer.toRightAnswer

    
    let leadingTeams n gameDay = 
        
        let q = PositiveNum.value n

        gameDay
        |> teams
        |> Seq.groupBy (fun t -> totalAnswered gameDay gameDay.PackageSize t)
        |> Seq.sortByDescending fst
        |> Seq.fold (fun res group -> 
                            if q > Seq.length res 
                            then group |> snd |> Seq.append res 
                            else res) Seq.empty

    /// 
    let getWinnerTeam gameDay = leadingTeams PositiveNum.numOne gameDay


    /// 
    let getDifficultQuestions threshold gameDay = 

        let isDifficult question = 
            
            let rightAnswers = rightAnswersOnQuestion gameDay question
            rightAnswers <= threshold


        gameDay.PackageSize
        |> PositiveNum.createNaturalRange
        |> Seq.filter isDifficult

    let getRatingOnDifficultQuestions threshold gameDay = 
        
        let questions = getDifficultQuestions threshold gameDay

        let answeredOnDifficultQuestions team = 
            
            questions
            |> Seq.map (getAnswer gameDay team)
            |> Seq.sumBy Answer.toRightAnswer

        gameDay
        |> teams
        |> Seq.map (fun t -> t, answeredOnDifficultQuestions t)
        |> Seq.sortByDescending (fun (_, a) -> a)

type SeasonTable = 
    {
        Results : Map<Team, decimal<Point> seq>
        Table : (Team * decimal<Point>) seq
        GamesCount : PositiveNum
    }

module SeasonTable = 
    
    let ofSeq data = 
        
        let create gamesCount = 
            {
                Table = data |> Seq.map (fun (team, rating) -> team, rating |> Seq.sum) |> Seq.sortByDescending snd
                Results = data |> Map.ofSeq
                GamesCount = gamesCount
            }

        data |> Seq.map (snd >> Seq.length) |> Seq.max |> PositiveNum.ofInt
        |> Result.map create


    let topNResult resultsToCount seasonTable = 
            
        let topResults allResults =
            
            let gamesToCount = 
                let t = 
                    if resultsToCount > seasonTable.GamesCount
                    then seasonTable.GamesCount
                    else resultsToCount
                t |> PositiveNum.value
            
            let topResults = 
                if gamesToCount > Seq.length allResults 
                then allResults
                else allResults |> Seq.sortByDescending id |> Seq.take gamesToCount
                    
            topResults |> Seq.sum

        seasonTable.Results
        |> Map.toSeq
        |> Seq.map (fun (team, results) -> team, topResults results)
        |> Seq.sortByDescending snd
        
        
        
type RoundOf12MatchUp = Team * Team

type QuarterFinalTeam =
    | RoundOf12Winner of RoundOf12MatchUp
    | Wildcard

type QuarterFinalMatchUp = QuarterFinalTeam * QuarterFinalTeam

type QuarterFinalWinner = QuarterFinalMatchUp

type SemifinalMatchUp = QuarterFinalWinner * QuarterFinalWinner

type SemifinalWinner = SemifinalMatchUp
type Final = SemifinalMatchUp * SemifinalMatchUp

module Playoff =
    
    let private splitByTwo s =
        
        let groupCount =
            let itemsCount = s |> Seq.length
            itemsCount / 2
        
        s |> Seq.take groupCount,
        s |> Seq.skip groupCount |> Seq.take groupCount
        
    
    let roundOf12Pairs (best12Teams : Team seq) =
            
            let top6, others = splitByTwo best12Teams 
                
            others
            |> Seq.rev
            |> Seq.zip top6
            |> Seq.map (fun (firstTeam, secondTeam) -> RoundOf12MatchUp(firstTeam, secondTeam)) 
            
            
    let quarterFinalPairs roundOf12Winners =
        
        let priorityFirst, prioritySecond =
            roundOf12Winners |> Seq.head, roundOf12Winners |> Seq.skip 1 |> Seq.head
            
        let firstHalf, secondHalf =
            roundOf12Winners |> Seq.skip 2 |> splitByTwo
            
        secondHalf
        |> Seq.rev
        |> Seq.zip firstHalf
        |> Seq.map (fun (one, two) -> QuarterFinalMatchUp(one, two)) 
        |> Seq.append
            [
                QuarterFinalMatchUp(priorityFirst, Wildcard);
                QuarterFinalMatchUp(prioritySecond, Wildcard)
            ]
            
    let semifinalPairs quarterFinalWinners =
        
        let firstGroup, secondGroup =
            quarterFinalWinners |> splitByTwo
            
        secondGroup
        |> Seq.rev
        |> Seq.zip firstGroup
        |> Seq.map (fun (firstTeam, secondTeam) -> SemifinalMatchUp(firstTeam, secondTeam))
        
    let final semifinalWinners = Final(Seq.head semifinalWinners, Seq.head semifinalWinners)
        
        
    
    let playOff best12Teams =
        
        best12Teams
        
        |> roundOf12Pairs 
        |> Seq.map RoundOf12Winner
        
        |> quarterFinalPairs
        |> Seq.map QuarterFinalWinner
        
        |> semifinalPairs
        |> Seq.map SemifinalWinner
        
        |> final
        
    let playoffString best12Teams =
        
        let sb = StringBuilder()
        sb += sprintf "Round of 12%s" Environment.NewLine
               
        let teamName (team : Team) = team.Name |>  NoEmptyString.value
        
        let roundOf12Pairs = 
            best12Teams
            |> roundOf12Pairs
            
        let printRoundOf12Pair num (firstTeam, secondTeam) = 
            let newPair = sprintf "R%d. %s vs %s%s" <| num + 1 <| teamName firstTeam <| teamName secondTeam <| Environment.NewLine 
            sb += newPair
            
        roundOf12Pairs
        |> Seq.iteri printRoundOf12Pair
        
        sb
        ++ Environment.NewLine
        ++ "Quarterfinal" ++ Environment.NewLine
        ++ "Q1. Winner (R1) vs wildcard2" ++ Environment.NewLine
        ++ "Q2. Winner (R2) vs wildcard1" ++ Environment.NewLine
        ++ "Q3. Winner (R3) vs Winner (R6)" ++ Environment.NewLine
        ++ "Q4. Winner (R4) vs Winner (R5)" ++ Environment.NewLine
        ++ Environment.NewLine
        ++ "Semifinal" ++ Environment.NewLine
        ++ "S1. Winner (Q1) vs Winner (Q4)" ++ Environment.NewLine
        ++ "S2. Winner (Q2) vs Winner (Q3)" ++ Environment.NewLine
        ++ Environment.NewLine
        ++ "Final" ++ Environment.NewLine
        ++ "Winner (S1) vs Winner (S2)" ++ Environment.NewLine
        ++ Environment.NewLine
        |> Out 