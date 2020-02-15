module SixtySeconds.Tests.ExtensionTests

open FsCheck
open NUnit.Framework
open FsCheck.NUnit

[<TestFixture>]
module SeqExceptLastExampleTests =
    
    open Utils
    open FsUnit
    
    // TODO maybe should throw exception?
    [<Test>]
    let ``Empty seq except last item is empty seq``() =
        
        Seq.empty
        |> Seq.exceptLast 
        |> should equal []
        
[<TestFixture>]
module SeqExceptLastPropertyBasedTests =
    
    open TestUtils
    open FsCheckUtils
    open Utils
    
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