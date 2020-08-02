namespace SixtySeconds.Infrastructure

module SixtySecondsApi =
    
    open SixtySeconds
    
    let gameDayRating arg =
           arg |> (SixtySecondsWorkflow.gameDayRating >> SixtySecondsProgramInterpreter.interpretSimple)
                   

    let teamBestPlace arg =
        arg |> (SixtySecondsWorkflow.teamBestPlace >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamWorstPlace arg =
        arg |> (SixtySecondsWorkflow.teamWorstPlace >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamBestStrike arg =
        arg |> (SixtySecondsWorkflow.teamBestStrike >> SixtySecondsProgramInterpreter.interpretSimple)
    
    let teamWorstStrike arg =
        arg |> (SixtySecondsWorkflow.teamWorstStrike >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamDifficultAnsweredQuestion arg =
        arg |> (SixtySecondsWorkflow.teamDifficultAnsweredQuestion >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamDifficultAnsweredQuestionCount arg =
        arg |> (SixtySecondsWorkflow.teamDifficultAnsweredQuestionCount >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamSimpleWrongAnsweredQuestion arg =
        arg |> (SixtySecondsWorkflow.teamSimpleWrongAnsweredQuestion >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamSimpleWrongAnsweredQuestionCount arg =
        arg |> (SixtySecondsWorkflow.teamSimpleWrongAnsweredQuestionCount >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let teamPerformance arg =
        arg |> (SixtySecondsWorkflow.teamPerformance >> SixtySecondsProgramInterpreter.interpretSimple)
        
    let parseTotal arg =
        arg |> (SixtySecondsWorkflow.parseTotal >> SixtySecondsProgramInterpreter.interpret)
        
    let parseGameDay arg =
        arg |> (SixtySecondsWorkflow.parseGameDay >> SixtySecondsProgramInterpreter.interpret)
        
    let showChart arg =
        arg |> (SixtySecondsWorkflow.showChart >> SixtySecondsProgramInterpreter.interpretSimple)