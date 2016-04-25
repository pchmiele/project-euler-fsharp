open System.Numerics
open System.IO

let euler13 = 
    let numbers = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler13.txt")
        File.ReadAllLines(path) 
        |> Seq.map BigInteger.Parse

    let sum = 
        numbers
        |> Seq.sum
        |> string 
        |> Seq.take 10
        |> Seq.toArray 
        |> System.String
    
    sum

printfn "%s" euler13