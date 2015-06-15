module Tests

open FsUnit
open Xunit
open NDA

[<Fact>]
let ``Given a sequence with unknown nucleotides, when filter, then remove unknown nucleotides`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let filtered = filterSequence nothing List.Empty sequence ' ' 0
    filtered |> System.String.Concat |> should equal "ATGAC"

[<Fact>]
let ``Given a sequence, when reverse, then return reverse sequence`` () =
    let sequence = "ATGAC".ToCharArray() |> Array.toList
    let filtered = filterSequence reverse List.Empty sequence ' ' 0
    filtered |> System.String.Concat |> should equal "CAGTA"
    
[<Fact>]
let ``Given a sequence with unknown nucleotides, when reverse, then return reverse sequence without unknown nucleotides`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let filtered = filterSequence reverse List.Empty sequence ' ' 0
    filtered |> System.String.Concat |> should equal "CAGTA"
