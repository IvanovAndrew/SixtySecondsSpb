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
            return team |> Team.bestPlace gameDay |> fst
        }
        
    let teamBestQuestion (team, gameDay) =
        simpleProgram{
            return team |> Team.bestPlace gameDay |> snd |> PositiveNum.value
        }
        
    let teamWorstPlace (team, gameDay) =
        simpleProgram{
            return team |> Team.worstPlace gameDay |> fst
        }
        
    let teamWorstQuestion (team, gameDay) =
        simpleProgram{
            return team |> Team.worstPlace gameDay |> snd |> PositiveNum.value
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
            return team |> Team.difficultAnswered gameDay |> fst |> PositiveNum.value
        }
        
    let teamDifficultAnsweredQuestionCount (team, gameDay) =
        simpleProgram{
            return team |> Team.difficultAnswered gameDay |> snd |> Converter.toInt
        }
        
    let teamSimpleWrongAnsweredQuestion (team, gameDay) =
        simpleProgram{
            return team |> Team.simplestWrongAnswered gameDay |> fst |> PositiveNum.value
        }
        
    let teamSimpleWrongAnsweredQuestionCount (team, gameDay) =
        simpleProgram{
            return team |> Team.simplestWrongAnswered gameDay |> snd |> Converter.toInt
        }