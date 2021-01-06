namespace SixtySeconds

open Actions
open Common.CommonTypes

module SixtySecondsWorkflow =
    
    open Domain
    open Actions
    open SixtySecondsProgramBuilder
    
    let parseTotal url =
        simpleProgram {
            return! parseSeasonRating url
        }
        
    let filterSeasonResults (options, results) =
        simpleProgram {
            return! topNResult options results
        }
        
    let parseGameDay (url, gameName) =
        simpleProgram{
            return! parseGameDay url gameName 
        }
    
    let gameDayRating (gameDay, ratingType) =
        simpleProgram {
        
            // TODO Move to action 
            let f =
                match ratingType with
                | All -> Rating.ofGameDay
                | Threshold threshold ->
                    fun gd -> 
                        let difficultQuestions = Question.getDifficultQuestions threshold gd 
                        gd |> Rating.ofGameDayWithFilter difficultQuestions
            
            return gameDay |> f |> List.ofSeq 
        }
        
        
    let teamBestPlace (team, gameDay) =
        simpleProgram{
            return team |> Team.bestPlace gameDay
        }
        
    let teamWorstPlace (team, gameDay) =
        simpleProgram{
            return team |> Team.worstPlace gameDay
        }
        
    let teamBestStrike (team, gameDay) =
        simpleProgram{
            return team |> Team.bestStrike gameDay
        }
        
    let teamWorstStrike (team, gameDay) =
        simpleProgram{
            return team |> Team.worstStrike gameDay
        }
        
    let teamDifficultAnsweredQuestion (team, gameDay) =
        simpleProgram{
            return team |> Team.difficultAnswered gameDay |> fst
        }
        
    let teamDifficultAnsweredQuestionCount (team, gameDay) =
        simpleProgram{
            return team |> Team.difficultAnswered gameDay |> snd |> Converter.toInt
        }
        
    let teamSimpleWrongAnsweredQuestion (team, gameDay) =
        simpleProgram{
            return team |> Team.simplestWrongAnswered gameDay |> fst
        }
        
    let teamSimpleWrongAnsweredQuestionCount (team, gameDay) =
        simpleProgram{
            return team |> Team.simplestWrongAnswered gameDay |> snd |> Converter.toInt
        }
        
    let teamPerformance (gameDay, team) =
        
        simpleProgram {
            let! bestPlace = teamBestPlace (team, gameDay)
            let! worstPlace = teamWorstPlace (team, gameDay)
            let! bestStrike = teamBestStrike (team, gameDay)
            let! worstStrike = teamWorstStrike (team, gameDay)
            let! difficultAnsweredQuestion = teamDifficultAnsweredQuestion (team, gameDay)
            let! difficultAnsweredQuestionCount = teamDifficultAnsweredQuestionCount (team, gameDay)
            let! simplestWrongAnsweredQuestion = teamSimpleWrongAnsweredQuestion (team, gameDay)
            let! simplestWrongAnsweredQuestionCount = teamSimpleWrongAnsweredQuestionCount (team, gameDay)
        
            return {
                        Team = team
                        BestPlace = bestPlace 
                        WorstPlace = worstPlace
                        BestStrike = bestStrike
                        WorstStrike = worstStrike
                        DifficultAnsweredQuestion = difficultAnsweredQuestion
                        DifficultAnsweredQuestionCount = difficultAnsweredQuestionCount
                        SimplestWrongAnsweredQuestion = simplestWrongAnsweredQuestion
                        SimplestWrongAnsweredQuestionCount = simplestWrongAnsweredQuestionCount
                    }
            }