
module NDA

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

let rec filterSequence func (filtered: char seq) sequence previousItem position = 
    match sequence with 
    | 'A'::tail -> filterSequence func (func filtered 'A' previousItem position) tail 'A' (position+1)
    | 'T'::tail -> filterSequence func (func filtered 'T' previousItem position) tail 'T' (position+1)
    | 'C'::tail -> filterSequence func (func filtered 'C' previousItem position) tail 'C' (position+1)
    | 'G'::tail -> filterSequence func (func filtered 'G' previousItem position) tail 'G' (position+1)
    | _::tail -> filterSequence func filtered tail previousItem (position+1)
    | [] -> filtered
