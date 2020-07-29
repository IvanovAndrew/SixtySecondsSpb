namespace Rating

module RatingTests = 

    
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    open TestUtils
    open TestUtils.FsCheckUtils
    open SixtySeconds.Common.CommonTypes

    open FsCheck
    open FsCheck.NUnit
    open PositiveNum
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<RatingGenerator>|])>]
    let ``Rating places don't intersect`` (seq : (Team * int) list) =
        
        let places = 
            seq
            |> Rating.ofSeq
            |> List.map (fun (_, _, place) -> place)
            
        let rec processPlace remainedPlaces = 
            let notIntersect one two =
                
                one.From < two.From && one.From < two.To ||
                two.From < one.From && two.From < one.To
            
            match remainedPlaces with
            | [] -> true
            | place :: others -> 
                
                let res = 
                    places
                    |> Seq.filter ((<>) place)
                    |> Seq.forall (notIntersect place)
                
                if res then processPlace others
                else false
        
        processPlace places
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<RatingGenerator>|])>]
    let ``Rating places hasn't gaps`` (seq : (Team * int) list) =
        
        let places = 
            seq
            |> Rating.ofSeq
            |> List.map (fun (_, _, place) -> place)
            |> Seq.distinct
            |> Seq.sort
            
        let totalRange = 
            
            places
            |> Seq.reduce (fun p1 p2 -> if PositiveNum.next p1.To = p2.From then {From = p1.From; To = p2.To} else p1)
            
        let teamsCount = seq |> Seq.length
            
        totalRange =
            {
                From = PositiveNum.numOne
                To = TestUtils.Utils.toPositiveNum teamsCount 
            }
            
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<RatingGenerator>|])>]
    let ``Team from the first place is always head of rating`` (seq : (Team * int) list) =
        
        let (_, _, place) = 
            seq
            |> Rating.ofSeq
            |> List.head
            
        place.From = PositiveNum.numOne
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Teams that give maximum right answers are on the first place`` gameDay =
        
        let teamWithMaximumRightAnswers =
            gameDay
            |> GameDay.teams
            |> Seq.maxBy (Team.totalAnswered gameDay gameDay.PackageSize)
            
        let leaderPlace =
            teamWithMaximumRightAnswers
            |> Team.getPlace gameDay 
        
        leaderPlace.From = PositiveNum.numOne
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<GameDayType>|])>]
    let ``Teams that give minimum right answers are on the last place`` gameDay =
        
        let teamWithMinimumAnswers =
            gameDay
            |> GameDay.teams
            |> Seq.minBy (Team.totalAnswered gameDay gameDay.PackageSize)
            
        let lastPlace = gameDay |> GameDay.teams |> Seq.length |> Utils.toPositiveNum
            
        let place = Team.getPlace gameDay teamWithMinimumAnswers
        place.To = lastPlace