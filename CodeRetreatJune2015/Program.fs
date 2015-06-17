// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open NDA
open ReadWrite
open System.IO

[<EntryPoint>]
let main argv = 
    let fileContent = File.ReadAllLines(argv.[0])
    let inputOutput, sequence = readInput fileContent
    
    let filtered = filterSequence' inputOutput (sequence |> Seq.toList)
    
    let file = new StreamWriter(argv.[0] + "_out")
    writeOutput filtered |> Seq.iter file.WriteLine
    file.Flush()
    0 // return an integer exit code
