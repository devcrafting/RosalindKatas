﻿module ReadWrite

open NDA

let (|Instruction|_|) name (instructionLine:string) =
    let parameters = instructionLine.Split(' ')
    if parameters.[0] = name
    then parameters |> Seq.skip 1 |> Seq.toArray |> Some
    else None

let readInput (fileContent: string array) =
        let input =
            if fileContent.Length > 1 then (fileContent.[0], fileContent.[1]) else ("nothing", fileContent.[0])
        let instruction, input = input
    
        match instruction with
            | "reverse" -> Reverse { Input = input; Output = List.Empty }
            | Instruction "count" parameters -> Count { Input = input; Pattern = parameters.[0].ToCharArray(); Output = (Seq.empty, 0) }
            //| "insert GA 4" -> Insert { Input = input; Insertion = ['G'; 'A']; Position = 4; Output = List.Empty }
            //| "complete" -> Complete { Input = input; Output = (Seq.empty, Seq.empty) }
            | _ -> Nothing { Input = input; Output = List.Empty }
