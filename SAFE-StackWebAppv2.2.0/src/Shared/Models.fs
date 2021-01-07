namespace Shared

open System

module Models =

    type TeamModel =
        {
            Id : int
            Name : string
        }

    type PlaceModel =
        {
            From : int
            To : int
        }

    type PlaceInfoModel = {Place : PlaceModel; Question : int}

    type TournamentModel =
        {
            City : string
            League : string
            Season : string
        }

    type AnswerOnQuestionModel = {Number : int; Answer : bool}
    type AnswersModel = AnswerOnQuestionModel list

    type GameDayModel =
        {
            Tournament : TournamentModel
            Name : string
            Answers : Map<TeamModel, AnswersModel>
            PackageSize : int
        }

    type GamedayPointsModel =
        {
            Date : DateTime
            Points : decimal
        }
        
    type SeasonResultModel = Map<TeamModel, GamedayPointsModel list>
        
    type TeamResultsTable = (TeamModel * decimal * PlaceModel) list
    type SeasonTableModel =
        {
            Results : SeasonResultModel
            Table : TeamResultsTable
            GamesCount : int
        }

    type FinalDateModel =
        | PlayedAlready of DateTime
        | NotPlayedYet
        
    type RatingOption =
        | FinalGameDoesntCount
        | FinalGameCounts 
    
    type RatingFilterModel =
        {
            GamesToCount : int
            FinalDate : FinalDateModel
            RatingOption : RatingOption
        }
        
    type GamedayRatingTypeModel =
        | All
        | Threshold of int
        
    

    type SheetOptions =
        {
            FirstQuestion : int

            TeamAnswered : string
            Answered : string
            Place : string
            Distance : string
        }

    type SpreadsheetOptions =
        {
            Id : string
            SheetName : string
            ColumnOptions : SheetOptions
        }

    type ShowChartsInput =
    | CustomTeamsOnly of int list
    | BestTeamsOnly of int
    | CustomTeamsAndBestTeams of teams : int list * bestTeams : int

    type ChartType =
        | Answers of ShowChartsInput
        | Places of ShowChartsInput
        
        
    