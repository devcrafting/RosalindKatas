
module NDA

open System.IO

type InputOutput = 
    | Nothing of Nothing
    | Reverse of Reverse
    | Count of Count
    | Insert of Insert
    | Complete of Complete
and Nothing = {
    Input: char seq;
    Output: char seq
}
and Reverse = {
    Input: char seq;
    Output: char seq
}
and Count = {
    Input: char seq;
    Pattern: char seq;
    Output: char seq * int
}
and Insert = {
    Input: char seq;
    Insertion: char seq;
    Position: int;
    Output: char seq
}
and Complete = {
    Input: char seq;
    Output: char seq * char seq
}

let nothing' filtered item =
    Seq.append filtered [item]
    
let reverse' filtered item =
    Seq.append [item] filtered

let count (pattern: char seq) (previousResult: char seq * int) item =
    let currentSequence = nothing' (fst previousResult) item
    let positionToStartCompare = (currentSequence |> Seq.length) - (pattern |> Seq.length)
    let comparableSegment = if positionToStartCompare > 0 then currentSequence |> Seq.skip positionToStartCompare else currentSequence
    let previousCount = snd previousResult 
    if comparableSegment |> Seq.toList = (pattern |> Seq.toList)
    then currentSequence, previousCount + 1
    else currentSequence, previousCount

let insert (insertion: char seq) position (previousResult: char seq) item =
    if position = (previousResult |> Seq.length)
    then
        let newItems = nothing' insertion item
        Seq.append previousResult newItems
    else nothing' previousResult item

let complete' (previousResult: char seq * char seq) item =
    let completion = 
        match item with
        | 'A' -> 'T'
        | 'T' -> 'A'
        | 'C' -> 'G'
        | 'G' -> 'C'
        | _ -> failwith "Unknow nucleotide - impossible ?!" 
    (nothing' (fst previousResult) item, nothing' (snd previousResult) completion)
    
let rec filterSequence' inputOutput sequence = 
    match sequence with 
    | head::tail when head = 'A' || head = 'T' || head = 'C' || head = 'G' -> 
        match inputOutput with
        | Nothing inputOutput -> 
            filterSequence' (Nothing { inputOutput with Output = nothing' inputOutput.Output head }) tail
        | Reverse inputOutput -> 
            filterSequence' (Reverse { inputOutput with Output = reverse' inputOutput.Output head }) tail
        | Count inputOutput ->
            filterSequence' (Count { inputOutput with Output = count inputOutput.Pattern inputOutput.Output head }) tail
        | Insert inputOutput ->
            filterSequence' (Insert { inputOutput with Output = insert inputOutput.Insertion inputOutput.Position inputOutput.Output head }) tail
        | Complete inputOutput ->
            filterSequence' (Complete { inputOutput with Output = complete' inputOutput.Output head }) tail
    | _::tail -> filterSequence' inputOutput tail
    | [] -> inputOutput

