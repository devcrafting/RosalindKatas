// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO

let nothing filtered item previousItem position =
    Seq.append filtered [item]

let reverse filtered item previousItem position =
    Seq.append [item] filtered

let countGA (filtered: char seq) item previousItem position =
    if previousItem = 'G' && item = 'A' 
    then 
        let list = filtered |> Seq.toList
        if list.Length = 0 
        then ['1'] |> List.toSeq 
        else [char ((int list.[0]) + 1)] |> List.toSeq 
    else filtered

let insertGA4 (filtered: char seq) item previousItem position =
    if position = 4
    then Seq.append filtered ['G'; 'A'; item ]
    else nothing filtered item previousItem position

let complete (filtered: char seq) item previousItem position =
    let completion = 
        match item with
        | 'A' -> 'T'
        | 'T' -> 'A'
        | 'C' -> 'G'
        | 'G' -> 'C'
        | _ -> ' '
    nothing filtered completion previousItem position

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
    let brins = input.ToCharArray() |> Array.toList
    let rec filterBrins (filtered: char seq) brins previousItem position = 
        match brins with 
        | 'A'::tail -> filterBrins (func filtered 'A' previousItem position) tail 'A' (position+1)
        | 'T'::tail -> filterBrins (func filtered 'T' previousItem position) tail 'T' (position+1)
        | 'C'::tail -> filterBrins (func filtered 'C' previousItem position) tail 'C' (position+1)
        | 'G'::tail -> filterBrins (func filtered 'G' previousItem position) tail 'G' (position+1)
        | _::tail -> filterBrins filtered tail previousItem (position+1)
        | [] -> filtered
    let filtered = filterBrins List.Empty brins ' ' 0
    let file = new StreamWriter(argv.[0] + "_out")
    if instruction = "complete"
    then file.WriteLine(input)
    else ()
    for brin in filtered do
        file.Write(brin)
    file.Flush()
    0 // return an integer exit code
