module Tests

open FsUnit
open Xunit
open NDA

[<Fact>]
let ``Given a sequence with unknown nucleotides, when filter, then remove unknown nucleotides`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let inputOutput = Nothing { Input = sequence; Output = List.Empty }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Nothing inputOutput -> inputOutput.Output |> System.String.Concat |> should equal "ATGAC"
    | _ -> raise (System.Exception "Result not expected")

[<Fact>]
let ``Given a sequence, when reverse, then return reverse sequence`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let inputOutput = Reverse { Input = sequence; Output = List.Empty }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Reverse inputOutput -> inputOutput.Output |> System.String.Concat |> should equal "CAGTA"
    
[<Fact>]
let ``Given a sequence with unknown nucleotides, when reverse, then return reverse sequence without unknown nucleotides`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let inputOutput = Reverse { Input = sequence; Output = List.Empty }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Reverse inputOutput -> inputOutput.Output |> System.String.Concat |> should equal "CAGTA"
    
[<Fact>]
let ``Given a sequence contains GA, when count GA, then return number of GA occurences`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let inputOutput = Count { Input = sequence; Pattern = ['G'; 'A']; Output = (Seq.empty,0) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Count inputOutput -> snd inputOutput.Output |> should equal 1
    
[<Fact>]
let ``Given a sequence contains several GA occurences, when count GA, then return number of GA occurences`` () =
    let sequence = "ATGACGA".ToCharArray() |> Array.toList
    let inputOutput = Count { Input = sequence; Pattern = ['G'; 'A']; Output = (Seq.empty,0) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Count inputOutput -> snd inputOutput.Output |> should equal 2
    
[<Fact>]
let ``Given a sequence contains ATG, when count ATG, then return number of ATG occurences`` () =
    let sequence = "ATGACGA".ToCharArray() |> Array.toList
    let inputOutput = Count { Input = sequence; Pattern = ['A'; 'T'; 'G']; Output = (Seq.empty,0) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Count inputOutput -> snd inputOutput.Output |> should equal 1
    
[<Fact>]
let ``Given a sequence, when insert GA at position 4, then return sequence with GA inserted at position 4`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let inputOutput = Insert { Input = sequence; Insertion = ['G'; 'A']; Position = 4; Output = Seq.empty }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Insert inputOutput -> inputOutput.Output |> System.String.Concat |> should equal "ATGAGAC"
    
[<Fact>]
let ``Given a sequence, when insert ATG at position 3, then return sequence with ATG inserted at position 3`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let inputOutput = Insert { Input = sequence; Insertion = ['A'; 'T'; 'G']; Position = 3; Output = Seq.empty }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Insert inputOutput -> inputOutput.Output |> System.String.Concat |> should equal "ATGATGAC"
    
[<Fact>]
let ``Given a sequence, when complete, then return original (filtered) and its completion`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let inputOutput = Complete { Input = sequence; Output = (Seq.empty, Seq.empty) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Complete inputOutput -> 
        fst inputOutput.Output |> System.String.Concat |> should equal "ATGAC"
        snd inputOutput.Output |> System.String.Concat |> should equal "TACTG"
