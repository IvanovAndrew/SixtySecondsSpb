namespace Playoff.Tests

open NUnit.Framework
open FsUnit


[<TestFixture>]
module PlayoffTests =
    
    open SixtySeconds.Domain
    open SixtySeconds.Actions
    open Utils
        
    let createTeams teamsCount =
        let createTeam id name =
            {
                ID = TestUtils.Utils.toPositiveNum id;
                Name = TestUtils.Utils.toNoEmptyString name 
            }
            
        let teamName num = sprintf "Team №%d" num
            
        (fun i -> i + 1)
        |> Seq.init teamsCount
        |> Seq.map (fun teamId -> createTeam teamId <| teamName teamId)
        
    [<Test>]
    let ``Round of 12 consists from 6 pairs``() =
        
        let teamsCount = 12
        
        teamsCount
        |> createTeams
        |> Playoff.roundOf12Pairs
        |> Seq.length
        |> should equal 6
        
    
    [<Test>]
    let ``All teams in round of 12 are different``() =
        
        let teamsCount = 12
        
        let pairs =
            teamsCount
            |> createTeams
            |> Playoff.roundOf12Pairs
        
        let collectUniqueTeams = Seq.distinct >> Seq.length
            
            
        pairs
        |> Seq.map (fun (first, second) -> [first;second;])
        |> Seq.concat
        |> collectUniqueTeams
        |> should equal 12
        
    [<Test>]
    let ``There are only two wildcard teams at quarterfinal stage``() =
        
        let teamsCount = 12
        
        let quarterFinalPairs = 
            teamsCount
            |> createTeams
            |> Playoff.roundOf12Pairs
            |> Seq.map RoundOf12Winner
            |> Playoff.quarterFinalPairs
        
        let count  =
            function
            | RoundOf12Winner _ -> 0
            | Wildcard -> 1
        
        quarterFinalPairs
        |> Seq.sumBy (fun (firstTeam, secondTeam) ->
                            let f = count firstTeam
                            let s = count secondTeam
                            s + f
                    )
        |> should equal 2