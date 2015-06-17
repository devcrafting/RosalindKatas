module ReadWrite

open NDA

let (|Instruction|_|) name (instructionLine:string) =
    let parameters = instructionLine.Split(' ')
    if parameters.[0] = name
    then parameters |> Seq.skip 1 |> Seq.toArray |> Some
    else None

let readInput (fileContent: string array) =
    let instruction, input = if fileContent.Length > 1 then (fileContent.[0], fileContent.[1]) else ("nothing", fileContent.[0])
    
    let inputOutput = match instruction with
        | "reverse" -> Reverse { Input = input; Output = List.Empty }
        | Instruction "count" parameters -> Count { Input = input; Pattern = parameters.[0].ToCharArray(); Output = (Seq.empty, 0) }
        | Instruction "insert" parameters -> Insert { Input = input; Insertion = parameters.[0].ToCharArray(); Position = int parameters.[1]; Output = List.Empty }
        | "complete" -> Complete { Input = input; Output = (Seq.empty, Seq.empty) }
        | _ -> Nothing { Input = input; Output = List.Empty }
    inputOutput, input