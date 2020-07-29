module SixtySeconds.Settings

type Setting =
    | TableUrl
    | SpreadsheetUrl
    | Game
    | FirstQuestion
    | TeamAnswered
    | RightAnswers
    | Place
    | Distance
    
    
module Config =
    
    open SixtySeconds.Views.Properties
    
    let private settingToKey = function
        | TableUrl -> "TableUrl"
        | SpreadsheetUrl -> "SpreadsheetUrl"
        | Game -> "Game"
        | FirstQuestion -> "FirstQuestion"
        | TeamAnswered -> "TeamAnswered"
        | RightAnswers -> "RightAnswers"
        | Place -> "Place"
        | Distance -> "Distance"
    
    let load setting =
        
        let getSettingValue key = Settings.Default.[key]
        
        setting
        |> settingToKey
        |> getSettingValue        
        |> unbox 
            
    let save setting value = 
        
        let key = settingToKey setting
        Settings.Default.[key] <- value 
        Settings.Default.Save()