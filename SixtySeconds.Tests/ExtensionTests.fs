namespace ExtensionTests

open FsCheck
open NUnit.Framework
open FsCheck.NUnit

[<TestFixture>]
module SeqExceptLastExampleTests =
    
    open Utils
    open FsUnit
    
    open SixtySeconds.Common.CommonTypes
    
    // TODO maybe should throw exception?
    [<Test>]
    let ``Empty seq except last item is empty seq``() =
        
        Seq.empty
        |> Seq.exceptLast 
        |> should equal []
        
module ``ExceptLast function specification`` = 
    
    open TestUtils
    open FsCheckUtils
    open Utils
    open SixtySeconds.Common.CommonTypes
    
    let nonEmptySeq =
        [1..10]
        |> Gen.elements 
        |> Gen.nonEmptyListOf
        |> Gen.map Seq.ofList
        |> Arb.fromGen
    
    // TODO benchmark implementation
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<NonEmptySeq>|])>]
    let ``Skipping the last item is the same as reverse seq, skip the first item and reverse again`` (seq : int seq) =
        
        let alternativeSeqResult = 
            seq
            |> Seq.rev
            |> Seq.skip 1
            |> Seq.rev
            |> List.ofSeq
        
        // convert to list because of this https://stackoverflow.com/questions/17101329/f-sequence-comparison
        let seqExceptLast = 
            seq
            |> Seq.exceptLast
            |> List.ofSeq
        
        seqExceptLast = alternativeSeqResult
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<NonEmptySeq>|])>]
    let ``Skipping the last item reduces sequence length by 1`` (seq : int seq) =
        
        let lengthBefore = seq |> Seq.length
        let lengthAfter = seq |> Seq.exceptLast |> Seq.length
        
        lengthBefore - lengthAfter = 1
        
module ``Result combine function specification`` =
    
    open TestUtils
    open FsCheckUtils
    open Utils
    open SixtySeconds.Common.CommonTypes
    
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<SeqResult>|])>]
    let ``Result.combine will be error if an error item exists``(seq : Result<int, string> list) =
        
        let isError = function Ok _ -> false | Error _ -> true
        
        (seq |> Seq.exists isError) ==> (seq |> Result.combine |> isError)
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<SeqOKResult>|])>]
    let ``Result.combine will be Ok if all items are Ok``(seq : Result<int, string> list) =
        
        (seq |> Seq.isEmpty |> not) ==> (seq |> Result.combine |> isOk)
        
    [<Property(QuietOnSuccess = true, Arbitrary = [|typeof<SeqOKResult>|])>]
    let ``if all items is OK then length of resulting sequence will be equal to items count``(seq : Result<int, string> list) =
        
        let itemsCount = seq |> Seq.length
        
        match Result.combine seq with
        | Ok newSeq -> newSeq |> Seq.length |> (=) itemsCount
        | Error _ -> false