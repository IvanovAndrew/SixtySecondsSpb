namespace SixtySeconds

module Actions = 

    open Domain
    open SixtySeconds.Common.CommonTypes
    open Utils
    open System.Text
    
    open System


    let createTeam id name =
        result {
            let! positiveId = PositiveNum.ofInt id
            let! noEmptyName = NoEmptyString.ofString name
            
            return {ID = positiveId; Name = noEmptyName;}
        }
        
    module Place =
        
        let toString place =
            if place.From = place.To then sprintf "%d" place.From.Value
            else sprintf "%d-%d" place.From.Value place.To.Value
            
            
    module Converter =
        
        let rightAnswerFromInt num = num * 1<RightAnswer>
        let pointFromDecimal num = num * 1m<Point>
        let toInt (num : int<_>) = int num
        let toDecimal (num : decimal<_>) = decimal num
        
    module GameDay =
        
        let allQuestions gameDay : QuestionNumber seq =
            gameDay.PackageSize
            |> PositiveNum.createNaturalRange
            |> Seq.ofList
            
        let withTeam team answers gameDay =
            
            let checkQuestionsCount a = 
                
                // TODO special error
                let count = a |> Answers.count
                if count = gameDay.PackageSize.Value then Ok() else Error "Questions count mismatching"

            let checkIfTeamAdded t = 
                
                let teamAdded = 
                    gameDay.Answers
                    |> Map.containsKey t

                // TODO special error
                if teamAdded then sprintf "Team %s is already added" <| t.Name.Value |> Error 
                else Ok()

            result{
                
                let! _ = checkQuestionsCount answers
                    
                let! _ = checkIfTeamAdded team
                
                let newAnswers = gameDay.Answers |> Map.add team answers
                return {gameDay with Answers = newAnswers}
            }
            
        /// Team played at game day
        let teams gameDay = 
            gameDay.Answers
            |> Map.toSeq
            |> Seq.map fst
        
        let answers gameDay =
            gameDay.Answers
            |> Map.toSeq
            |> Seq.map snd
            
    module Question =
        
        /// Number of teams that correctly answered on question 
        let rightAnswers gameDay question = 
            gameDay
            |> GameDay.answers
            |> Seq.map (Answers.getAnswer question)
            |> Answers.ofSeq
            |> Answers.sumRightAnswers
            
        /// 
        let getDifficultQuestions threshold gameDay = 

            let isDifficult question = 
                
                let rightAnswers = rightAnswers gameDay question
                rightAnswers <= threshold

            gameDay
            |> GameDay.allQuestions
            |> Seq.filter isDifficult
            
        let findDifficultQuestion gameDay =
            
            gameDay
            |> GameDay.allQuestions
            |> Seq.sortBy (rightAnswers gameDay)
            |> Seq.head

        let findDifficultQuestionWithRightAnswer gameDay =
            
            gameDay
            |> GameDay.allQuestions
            |> Seq.filter (fun q -> rightAnswers gameDay q > 0<RightAnswer>)
            |> Seq.sortBy (fun q -> rightAnswers gameDay q)
            |> Seq.head

    module Rating =
        
        let ofSeq (input : ((Team * _) seq)) =
            
            let proc (acc, fromPlace) group =
                
                let place =
                    let toPlace =
                    
                        let groupLength = group |> Seq.length
                        if groupLength <= 1 then fromPlace
                        else
                            groupLength - 1
                            |> PositiveNum.ofInt
                            |> Result.valueOrException
                            |> PositiveNum.add fromPlace 
                
                    {From = fromPlace; To = toPlace}
                    
                
                let newGroup = 
                    group
                    |> Seq.map (fun (team, rating) -> team, rating, place)
                    |> Seq.append acc
                    
                newGroup, PositiveNum.next place.To
                
            input
            |> Seq.groupBy (fun (_, rating) -> rating)
            |> Seq.sortByDescending (fun (key, _) -> key)
            |> Seq.map snd
            |> Seq.fold proc (Seq.empty, PositiveNum.numOne)
            |> fst
            |> List.ofSeq

        let ofGameDay gameDay : GameDayRating =
            gameDay
            |> GameDay.teams
            |> Seq.map (fun team -> team, gameDay.Answers.[team] |> Answers.sumRightAnswers)
            |> ofSeq
            
        let ofGameDayWithFilter questions gameDay : GameDayRating = 
            
            let answeredOnDifficultQuestions team = 
                
                gameDay.Answers
                |> Map.find team
                |> Answers.filterByQuestionNumber questions
                |> Answers.sumRightAnswers

            gameDay
            |> GameDay.teams
            |> Seq.map (fun t -> t, answeredOnDifficultQuestions t)
            |> ofSeq
            
        let leadingTeams topN rating = 
            
            rating
            |> Seq.takeWhile (fun (_, _, place) -> place.From <= topN)
            |> Seq.map (fun (team, _, _) -> team)

        /// 
        let getWinnerTeam rating = leadingTeams PositiveNum.numOne rating
        
    module Team =
        
        /// Did team A give right answer question Q
        let getAnswer gameDay questionNumber team = 
                
            gameDay.Answers 
            |> Map.find team
            |> Answers.getAnswer questionNumber

        let bestStrike gameDay team =
            gameDay.Answers
            |> Map.find team
            |> Answers.findStrike Best  
        
        let worstStrike gameDay team =
            
            gameDay.Answers
            |> Map.find team
            |> Answers.findStrike Worst
            
        
            
        let totalAnswered gameDay (questionNumber : QuestionNumber) team  = 
            
            gameDay.Answers
            |> Map.find team
            |> Answers.takeFirst questionNumber
            |> Answers.sumRightAnswers
            
        /// Team position after question Q
        let getPlaceAfterQuestion gameDay team questionNumber = 
            
            let totalAnswered' = totalAnswered gameDay questionNumber 

            let rating = 
                gameDay
                |> GameDay.teams
                |> Seq.map (fun team -> team, totalAnswered' team)
                |> Rating.ofSeq
            
            let (_, _, place) = 
                rating
                |> List.find (fun (t, _, _) -> t = team)
            place
            
        let getPlace gameDay team =
            getPlaceAfterQuestion gameDay team gameDay.PackageSize
        
        /// Отставание команды A от лидера по состоянию на вопрос Q
        let getGapFromTheFirstPlace gameDay team questionNumber = 
            
            // get rating
            // find team
            // find leader
            
            let totalAnswered' = totalAnswered gameDay questionNumber
            let teamAnsweredOn = totalAnswered' team 

            let leaderAnsweredOn = 
                gameDay
                |> GameDay.teams
                |> Seq.map totalAnswered'
                |> Seq.max
            
            teamAnsweredOn - leaderAnsweredOn
            
        let private teamPlaces gameDay team =
            
            let getPlaceAfterQuestion' = getPlaceAfterQuestion gameDay team
            
            gameDay
            |> GameDay.allQuestions
            |> Seq.map (fun q -> q, getPlaceAfterQuestion' q)
            
        
        let bestPlace gameDay team =
        
            let minimumQuestions = 6 |> PositiveNum.ofInt |> Result.valueOrException
        
            let question, place = 
                team
                |> teamPlaces gameDay
                |> Seq.filter (fun (q, _) -> q >= minimumQuestions)
                |> Seq.minBy snd
             
            place, question 

        let worstPlace gameDay team =
            
            let minimumQuestions = 6 |> PositiveNum.ofInt |> Result.valueOrException 
            
            let question, place = 
                team
                |> teamPlaces gameDay
                |> Seq.filter (fun (q, _) -> q >= minimumQuestions)
                |> Seq.maxBy snd
            
            place, question        
        
        let difficultAnswered gameDay team =
            
            gameDay.Answers
            |> Map.find team
            |> Answers.filter Right
            |> Seq.map (fun q -> q, q |> Question.rightAnswers gameDay)
            |> Seq.minBy snd
            
        let simplestWrongAnswered gameDay team =
            
            gameDay.Answers
            |> Map.find team
            |> Answers.filter Wrong
            |> Seq.map (fun q -> q, q |> Question.rightAnswers gameDay)
            |> Seq.maxBy snd
            
    module SeasonTable = 
        
        let ofSeq data = 
            
            let create gamesCount = 
                
                let table = 
                    
                    data
                    |> Seq.map (fun (team, rating) -> team, rating |> Seq.sum)
                    |> Rating.ofSeq
                
                {
                    Table = table 
                    Results = data |> Map.ofSeq
                    GamesCount = gamesCount
                }
                
            let gamesCount d =
                d
                |> Seq.map (snd >> Seq.length)
                |> Seq.max
                |> PositiveNum.ofInt

            data
            |> gamesCount
            |> Result.map create


        let topNResult resultsToCount seasonTable : SeasonRating = 
                
            let topResults allResults =
                
                let gamesToCount = 
                    let t = min resultsToCount seasonTable.GamesCount
                    t.Value
                
                allResults
                |> Seq.sortByDescending id 
                |> Seq.take gamesToCount
                |> Seq.sum
            
            seasonTable.Results
            |> Map.toSeq
            |> Seq.map (fun (team, results) -> team, topResults results)
            |> Rating.ofSeq
            
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
                   
            let teamName (team : Team) = team.Name.Value
            
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