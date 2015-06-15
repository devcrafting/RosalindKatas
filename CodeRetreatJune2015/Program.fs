// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open NDA
open System.IO

[<EntryPoint>]
let main argv = 
    let fileContent = File.ReadAllLines(argv.[0])
    let input =
        if fileContent.Length > 1 then (fileContent.[0], fileContent.[1]) else ("nothing", fileContent.[0])
    let instruction, input = input
    
    let func = 
        match instruction with
        | "reverse" -> reverse
        | "count GA" -> countGA
        | "insert GA 4" -> insertGA4
        | "complete" -> complete
        | _ -> nothing
    let sequence = input.ToCharArray() |> Array.toList
    
    let filtered = filterSequence func List.Empty sequence ' ' 0
    
    let file = new StreamWriter(argv.[0] + "_out")
    if instruction = "complete"
    then file.WriteLine(input)
    else ()
    for brin in filtered do
        file.Write(brin)
    file.Flush()
    0 // return an integer exit code
