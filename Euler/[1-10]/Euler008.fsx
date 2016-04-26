open System.IO

let euler8 = 
    let numbers = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler8.txt")
        File.ReadAllLines(path)
        |> Seq.concat
        |> Seq.map (fun x -> int64(x.ToString()))

    let calcProduct numbers = 
        numbers
        |> Seq.fold (fun acc x -> (acc * x) ) 1L
    
    let maxProduct =
        numbers
        |> Seq.windowed(13)
        |> Seq.map (fun x -> calcProduct(x) ) 
        |> Seq.max
    
    maxProduct

printfn "%d" euler8