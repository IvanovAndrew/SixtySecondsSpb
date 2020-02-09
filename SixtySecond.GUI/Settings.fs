module SixtySecond.GUI.Settings

open Utils

type Setting =
    | TableUrl
    | SpreadsheetUrl
    | FirstQuestion
    | TeamAnswered
    | RightAnswers
    | Place
    | Distance
    
    
module Config =
    
    let private settingToKey = function
        | TableUrl -> "TableUrl"
        | SpreadsheetUrl -> "SpreadsheetUrl"
        | FirstQuestion -> "FirstQuestion"
        | TeamAnswered -> "TeamAnswered"
        | RightAnswers -> "RightAnswers"
        | Place -> "Place"
        | Distance -> "Distance"
    
    let load setting =
        
        let getSettingValue key =
            SixtySeconds.Views.Properties.Settings.Default.[key]
        
        setting
        |> settingToKey
        |> getSettingValue        
        |> unbox 
            
    let save setting value = 
        
        let key = settingToKey setting
        SixtySeconds.Views.Properties.Settings.Default.[key] <- value 
        SixtySeconds.Views.Properties.Settings.Default.Save()