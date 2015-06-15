module Tests

open FsUnit
open Xunit
open NDA

[<Fact>]
let ``Given a sequence with unknown nucleotides, when filter, then remove unknown nucleotides`` () =
    let sequence = "ATUGACRE".ToCharArray() |> Array.toList
    let filtered = filterSequence nothing List.Empty sequence ' ' 0
    filtered |> System.String.Concat |> should equal "ATGAC"