module ReadWriteTests

open FsUnit
open Xunit
open ReadWrite
open NDA

[<Fact>]
let ``Given input without instruction, when read input, then return Nothing InputOutput`` () =
    let input = [|"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Nothing inputOutput -> 
        inputOutput.Input |> should equal input.[0]
        inputOutput.Output |> Seq.length |> should equal 0
        
[<Fact>]
let ``Given input with Reverse instruction, when read input, then return Reverse InputOutput`` () =
    let input = [|"reverse";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Reverse inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        inputOutput.Output |> Seq.length |> should equal 0
        
[<Fact>]
let ``Given input with Count instruction with pattern GA, when read input, then return Count InputOutput`` () =
    let input = [|"count GA";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Count inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        inputOutput.Pattern |> should equal [|'G';'A'|]
        fst inputOutput.Output |> Seq.length |> should equal 0
        snd inputOutput.Output |> should equal 0
        
[<Fact>]
let ``Given input with Count instruction with pattern TC, when read input, then return Count InputOutput`` () =
    let input = [|"count TC";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Count inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        inputOutput.Pattern |> should equal [|'T';'C'|]
        fst inputOutput.Output |> Seq.length |> should equal 0
        snd inputOutput.Output |> should equal 0
        
[<Fact>]
let ``Given input with Insert instruction with insertion AG at position 4, when read input, then return Insert InputOutput`` () =
    let input = [|"insert GA 4";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Insert inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        inputOutput.Insertion |> should equal [|'G';'A'|]
        inputOutput.Position |> should equal 4
        inputOutput.Output |> Seq.length |> should equal 0
             
[<Fact>]
let ``Given input with Insert instruction with insertion CT at position 2, when read input, then return Insert InputOutput`` () =
    let input = [|"insert CT 2";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Insert inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        inputOutput.Insertion |> should equal [|'C';'T'|]
        inputOutput.Position |> should equal 2
        inputOutput.Output |> Seq.length |> should equal 0
         
[<Fact>]
let ``Given input with Complete instruction, when read input, then return Complete InputOutput`` () =
    let input = [|"complete";"ACGTC"|]
    let inputOutput, sequence = readInput input
    match inputOutput with
    | Complete inputOutput -> 
        inputOutput.Input |> should equal input.[1]
        fst inputOutput.Output |> Seq.length |> should equal 0
        snd inputOutput.Output |> Seq.length |> should equal 0
        
[<Fact>]
let ``Given output of Nothing instruction, when write output, then return list of lines to write`` () =
    let inputOutput = Nothing { Input = "ACTG"; Output = "TGAC" }
    let outputLines = writeOutput inputOutput
    outputLines |> should equal ["TGAC"]
    
[<Fact>]
let ``Given output of Reverse instruction, when write output, then return list of lines to write`` () =
    let inputOutput = Reverse { Input = "ACTG"; Output = "GTCA" }
    let outputLines = writeOutput inputOutput
    outputLines |> should equal ["GTCA"]
    
[<Fact>]
let ``Given output of Count instruction, when write output, then return list of lines to write`` () =
    let inputOutput = Count { Input = "ACTG"; Pattern = "AC"; Output = ("ACTG".ToCharArray() |> Array.toSeq, 1) }
    let outputLines = writeOutput inputOutput
    outputLines |> should equal ["1"]
    
[<Fact>]
let ``Given output of Insert instruction, when write output, then return list of lines to write`` () =
    let inputOutput = Insert { Input = "ACTG"; Insertion = "AC"; Position = 2; Output = "ACACTG" }
    let outputLines = writeOutput inputOutput
    outputLines |> should equal ["ACACTG"]
    
[<Fact>]
let ``Given output of Complete instruction, when write output, then return list of lines to write`` () =
    let inputOutput = Complete { Input = "ACTG"; Output = ("ACTG".ToCharArray() |> Array.toSeq,"TGAC".ToCharArray() |> Array.toSeq) }
    let outputLines = writeOutput inputOutput
    outputLines |> should equal ["ACTG";"TGAC"]