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
let ``Given a sequence, when count GA, then return number of GA occurences`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let inputOutput = Count { Input = sequence; Pattern = ['G'; 'A']; Output = (Seq.empty,0) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Count inputOutput -> snd inputOutput.Output |> should equal 1
    
[<Fact>]
let ``Given a sequence contains several occurences, when count GA, then return number of GA occurences`` () =
    let sequence = "ATGACGA".ToCharArray() |> Array.toList
    let inputOutput = Count { Input = sequence; Pattern = ['G'; 'A']; Output = (Seq.empty,0) }
    let filtered = filterSequence' inputOutput sequence
    match filtered with
    | Count inputOutput -> snd inputOutput.Output |> should equal 2
    