module ReadWrite

open NDA

let readInput (fileContent: string array) =
        let input =
            if fileContent.Length > 1 then (fileContent.[0], fileContent.[1]) else ("nothing", fileContent.[0])
        let instruction, input = input
    
        match instruction with
            | "reverse" -> Reverse { Input = input; Output = List.Empty }
            | "count GA" -> Count { Input = input; Pattern = ['G'; 'A']; Output = (Seq.empty, 0) }
            | _ -> Nothing { Input = input; Output = List.Empty }
