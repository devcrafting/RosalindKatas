// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO

[<EntryPoint>]
let main argv = 
    let fileContent = File.ReadAllText(argv.[0])
    File.WriteAllText(argv.[0] + "_out", fileContent)
    0 // return an integer exit code
