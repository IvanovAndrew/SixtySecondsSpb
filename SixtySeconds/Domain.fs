namespace SixtySeconds

open System

module Domain = 

    open SixtySeconds.Common.CommonTypes


    type Team = 
        {
            ID : PositiveNum
            Name : NoEmptyString
        }

    type Place = 
        {
            From : PositiveNum
            To : PositiveNum
        }    
    
    [<Measure>]
    type RightAnswer
        
    type Answer = 
        | Right 
        | Wrong
        
        with
            static member ofDecimal num = if num > 0m then Right else Wrong
            static member ofBool isRight = if isRight then Right else Wrong
            static member isRight answer = answer = Answer.Right
            static member toRightAnswer answer = if Answer.isRight answer then 1<RightAnswer> else 0<RightAnswer>
        
    

    [<Measure>]
    type Point 

    type StrikeType =
        | Best
        | Worst
        
    type QuestionNumber = PositiveNum
    
    type StrikeInfo =
        {
            Type : StrikeType
         //   From : QuestionNumber
         //   To : QuestionNumber
            Count : PositiveNum option
        }
        with
            static member Default strikeType = {Type = strikeType; Count = None}
            static member Increase strike =
                {
                    strike with
                        Count =
                            strike.Count
                            |> Option.map (PositiveNum.next)
                            |> Option.orElse (Some PositiveNum.numOne) 
                }
    
    type AnswerOnQuestion = {Number : QuestionNumber; Answer : Answer}
    type Answers = private Answers of (AnswerOnQuestion) list
        with
            static member ofSeq seq =

                let indexToQuestionNumber index : QuestionNumber =
                    index + 1 |> PositiveNum.ofInt |> Result.valueOrException

                seq
                |> Seq.mapi (fun i answer -> {Number = indexToQuestionNumber i; Answer = answer})
                |> List.ofSeq
                |> Answers 
        
            static member ofBoolSeq seq = seq |> Seq.map Answer.ofBool |> Answers.ofSeq
        
            static member ofAnswersList seq = Answers seq
            
            static member getAnswer questionNumber (Answers answers) = 
                
                let answerOnQuestion =
                    answers
                    |> Seq.find (fun item -> item.Number = questionNumber)
                answerOnQuestion.Answer
                

            static member count (Answers answers) = answers |> Seq.length 
            static member filter answer  (Answers answers) =
                
                answers
                |> List.filter (fun item -> item.Answer = answer)
                |> List.map (fun item -> item.Number)
                
            static member filterByQuestionNumber questions (Answers answers) =
                
                answers
                |> List.filter (fun answerOnQuestion -> questions |> Seq.exists ((=) answerOnQuestion.Number))
                |> Answers.ofAnswersList
            
            
            static member takeFirst questionNumber (Answers answers) =
                
                answers
                |> List.filter (fun answerOnQuestion -> answerOnQuestion.Number <= questionNumber)
                |> Answers.ofAnswersList
                
               
            static member findStrike strikeType (Answers answers) =
                
                let defaultStrike = StrikeInfo.Default strikeType
                let findBestStrike current best =
                    match current.Count, best.Count with
                    | (Some c, Some b) when c > b -> current
                    | (Some c, Some b) -> best
                    | None, Some _ -> best
                    | Some _, None -> current
                    | _ -> best
                
                let f =
                    fun (current, bestStrike) answer ->
                        match strikeType, answer with
                        | Best, Right
                        | Worst, Wrong ->
                            let newCurrent = StrikeInfo.Increase current
                            newCurrent, findBestStrike newCurrent bestStrike
                        | _ -> defaultStrike, bestStrike
                
                let strike = 
                    answers
                    |> Seq.map (fun answerOnQuestion -> answerOnQuestion.Answer)
                    |> Seq.fold f (defaultStrike, defaultStrike)
                    |> snd
                
                strike
                
            static member sumRightAnswers (Answers answers) =
                answers
                |> Seq.sumBy (fun answerOnQuestion -> answerOnQuestion.Answer |> Answer.toRightAnswer)
                
            static member toList (Answers answers) = answers
            
    type CityName = NoEmptyString
    type LeagueName = NoEmptyString
    type GameName = NoEmptyString
    type SeasonName = NoEmptyString
    
    type Tournament =
        {
            City : CityName
            League : LeagueName
            Season : SeasonName
        }
        
    type FinalDate =
        | NotPlayedYet
        | AlreadyPlayed of DateTime
        
    type RatingOption =
        | FinalGameDoesntCount
        | FinalGameCounts 
        
    type SeasonResultFilter =
        {
            GamesToCount : PositiveNum
            FinalDate : FinalDate
            RatingOption : RatingOption
        }
        

    type GameDay = 
        {
            Tournament : Tournament
            Name : GameName
            Answers : Map<Team, Answers>
            PackageSize : PositiveNum
        }
        
    type RatingType =
        | All
        | Threshold of int<RightAnswer>
    
    type GamedayPointWithAttendance =
        | Missed
        | Played of decimal<Point>
    
    type GamedayPoint =
        {
            Date : DateTime
            Point : GamedayPointWithAttendance 
        }
    
    type RatingPoints = GamedayPoint list    
        
    type TeamRatingPosition<'a> = Team * 'a * Place
    type GameDayRating = TeamRatingPosition<int<RightAnswer>> list
    type SeasonRating = TeamRatingPosition<decimal<Point>> list
    
    type SeasonResults = Map<Team, RatingPoints>
    type SixtySecondsSeason = SeasonResults
    type MatrixSeason = SeasonResults
    
    type SeasonTable = 
        {
            Results : SixtySecondsSeason
            Table : SeasonRating
            GamesCount : PositiveNum
        }
        
    type PlaceInfo = {Place : Place; Question : QuestionNumber}
        
    type TeamPerformance =
        {
            Team : Team
            BestPlace : PlaceInfo
            WorstPlace : PlaceInfo
            BestStrike : StrikeInfo
            WorstStrike : StrikeInfo
            DifficultAnsweredQuestion : QuestionNumber
            DifficultAnsweredQuestionCount : int
            SimplestWrongAnsweredQuestion : QuestionNumber
            SimplestWrongAnsweredQuestionCount : int
        }

    type RoundOf12MatchUp = Team * Team

    type QuarterFinalTeam =
        | RoundOf12Winner of RoundOf12MatchUp
        | Wildcard

    type QuarterFinalMatchUp = QuarterFinalTeam * QuarterFinalTeam

    type QuarterFinalWinner = QuarterFinalMatchUp

    type SemifinalMatchUp = QuarterFinalWinner * QuarterFinalWinner

    type SemifinalWinner = SemifinalMatchUp
    type Final = SemifinalMatchUp * SemifinalMatchUp