namespace Shared

open Models
open Shared

type RatingType =
    | All
    | Threshold of int

type TeamRatingPosition<'a> = TeamModel * 'a * PlaceModel
type GameDayRating = TeamRatingPosition<int> list
type SeasonRating = TeamRatingPosition<decimal> list

type StrikeType =
        | Best
        | Worst

type StrikeInfo =
        {
            Type : StrikeType
         //   From : QuestionNumber
         //   To : QuestionNumber
            Count : int option
        }
        static member Default strikeType = {Type = strikeType; Count = None}
        static member Increase strike =
            {
                strike with
                    Count =
                        strike.Count
                        |> Option.map (fun i -> i+1)
                        |> Option.orElse (Some 1)
            }

//type PlaceInfo = {Place : Place; Question : int}

type TeamPerformance =
    {
        Team : TeamModel
        BestPlace : PlaceInfoModel
        WorstPlace : PlaceInfoModel
        BestStrike : StrikeInfo
        WorstStrike : StrikeInfo
        DifficultAnsweredQuestion : int
        DifficultAnsweredQuestionCount : int
        SimplestWrongAnsweredQuestion : int
        SimplestWrongAnsweredQuestionCount : int
    }

type SeasonTable =
    {
        Results : Map<TeamModel, decimal seq>
        Table : SeasonRating
        GamesCount : int
    }

module Place =

    let toString (place : PlaceModel) =
        if place.From = place.To then string place.From
        else sprintf "%d-%d" place.From place.To

module GameDay =  /// Team played at game day
    let teams (gameDay : GameDayModel) =
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map fst

    let answers gameDay =
        gameDay.Answers
        |> Map.toSeq
        |> Seq.map snd

    let allQuestions gameDay = [1..gameDay.PackageSize]

    let getAnswer questionNumber answers =

        let answerOnQuestion =
            answers
            |> Seq.find (fun item -> item.Number = questionNumber)
        answerOnQuestion.Answer

    /// Number of teams that correctly answered on question
    let rightAnswers gameDay question =
        gameDay
        |> answers
        |> Seq.map (getAnswer question)
        |> Seq.filter id
        |> Seq.length

    let toTriple gameDay =
        (
            (gameDay.Tournament.City, gameDay.Tournament.League, gameDay.Tournament.Season),
            gameDay.Answers |> Map.toList |> List.map (fun (team, answers) -> (team.Id, team.Name), ())
        )

type ShowChartsInput =
    | CustomTeamsOnly of TeamModel list
    | BestTeamsOnly of int
    | CustomTeamsAndBestTeams of teams : TeamModel list * bestTeams : int

type ChartType =
    | Answers of ShowChartsInput
    | Places of ShowChartsInput

module Rating =

    let ofSeq (input : ((TeamModel * _) seq)) =

        let proc (acc, fromPlace) group =

            let place =
                let toPlace =

                    let groupLength = group |> Seq.length
                    if groupLength <= 1 then fromPlace
                    else
                        (groupLength - 1) + fromPlace

                {From = fromPlace; To = toPlace}


            let newGroup =
                group
                |> Seq.map (fun (team, rating) -> team, rating, place)
                |> Seq.append acc

            newGroup, place.To + 1

        input
        |> Seq.groupBy (fun (_, rating) -> rating)
        |> Seq.sortByDescending (fun (key, _) -> key)
        |> Seq.map snd
        |> Seq.fold proc (Seq.empty, 1)
        |> fst
        |> List.ofSeq

    let ofGameDay gameDay : GameDayRating =
        gameDay
        |> GameDay.teams
        |> Seq.map (fun team -> team, gameDay.Answers.[team] |> Seq.filter (fun a -> a.Answer) |> Seq.length )
        |> ofSeq

    let topNResult (resultsToCount : int) (seasonTable : SeasonTableModel) : SeasonRating =

            let topResults allResults =

                let gamesToCount =
                    let playedGames = allResults |> Seq.length
                    min playedGames resultsToCount

                allResults
                |> Seq.sortByDescending id
                |> Seq.take gamesToCount
                |> Seq.sum

            seasonTable.Results
            |> Map.toSeq
            |> Seq.map (fun (team, results) -> team, topResults results)
            |> ofSeq

//    let ofGameDayWithFilter questions gameDay : GameDayRating =
//
//        let answeredOnDifficultQuestions team =
//
//            gameDay.Answers
//            |> Map.find team
//            |> Answers.filterByQuestionNumber questions
//            |> Answers.sumRightAnswers
//
//        gameDay
//        |> GameDay.teams
//        |> Seq.map (fun t -> t, answeredOnDifficultQuestions t)
//        |> ofSeq

    let leadingTeams topN rating =

        rating
        |> Seq.takeWhile (fun (_, _, place) -> place.From <= topN)
        |> Seq.map (fun (team, _, _) -> team)

    ///
    let getWinnerTeam rating = leadingTeams 1 rating




module Team =

    let private findStrike strikeType answers =

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
                | Best, true
                | Worst, false ->
                    let newCurrent = StrikeInfo.Increase current
                    newCurrent, findBestStrike newCurrent bestStrike
                | _ -> defaultStrike, bestStrike

        let strike =
            answers
            |> Seq.map (fun answerOnQuestion -> answerOnQuestion.Answer)
            |> Seq.fold f (defaultStrike, defaultStrike)
            |> snd

        strike

    let private rightAnswers gameDay question =
        gameDay
        |> GameDay.answers
        |> Seq.map (fun aqa -> aqa |> Seq.find (fun aq -> aq.Number = question))
        |> Seq.sumBy (fun aq -> if aq.Answer then 1 else 0)

    /// Did team A give right answer question Q
    let getAnswer gameDay questionNumber team =

        gameDay.Answers
        |> Map.find team
        |> Seq.find (fun aq -> aq.Number = questionNumber)

    let bestStrike gameDay team =
        gameDay.Answers
        |> Map.find team
        |> findStrike Best

    let worstStrike gameDay team =

        gameDay.Answers
        |> Map.find team
        |> findStrike Worst

    let totalAnswered gameDay (questionNumber : int) team  =

        gameDay.Answers
        |> Map.find team
        |> Seq.where (fun aq -> aq.Number <= questionNumber)
        |> Seq.sumBy (fun aq -> if aq.Answer then 1 else 0)

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

        let minimumQuestions = if gameDay.PackageSize > 12 then 6 else gameDay.PackageSize / 2

        let question, place =
            team
            |> teamPlaces gameDay
            |> Seq.filter (fun (q, _) -> q >= minimumQuestions)
            |> Seq.minBy snd

        {Place = place; Question = question}

    // TODO almost duplicate bestPlace function 
    let worstPlace gameDay team =

        let minimumQuestions = if gameDay.PackageSize > 12 then 6 else gameDay.PackageSize / 2 

        let question, place =
            team
            |> teamPlaces gameDay
            |> Seq.filter (fun (q, _) -> q >= minimumQuestions)
            |> Seq.maxBy snd

        {Place = place; Question = question}

    let difficultAnswered gameDay team =

        gameDay.Answers
        |> Map.find team
        |> Seq.filter (fun aq -> aq.Answer)
        |> Seq.map (fun aq -> aq, aq.Number |> rightAnswers gameDay)
        |> Seq.minBy snd

    let simplestWrongAnswered gameDay team =

        gameDay.Answers
        |> Map.find team
        |> Seq.filter (fun aq -> not aq.Answer)
        |> Seq.map (fun q -> q, q.Number |> rightAnswers gameDay)
        |> Seq.maxBy snd