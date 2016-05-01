open System.IO

let euler67 =   
    let numbers = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler67.txt")
        File.ReadAllLines(path) 
        |> Seq.map (fun line -> line.Split(' ') |> Seq.map int32 |> Seq.toList )
        |> Seq.toList
        |> List.rev
    
    let maxOfEvery2 collection = 
        Seq.fold (fun max x -> if x > max then x else max ) 0 collection

    let partialMaxPath first second =
        let maxOfEvery2 = 
            first
            |> List.windowed 2 
            |> List.map maxOfEvery2
        
        List.map2 (fun x y -> x + y) second maxOfEvery2

    let rec maxPath (numbers:list<int32 list>) =
        match numbers with
            | [] -> None
            | [head] -> Some(head)
            | first :: second :: tail -> 
                let newRow = partialMaxPath first second
                maxPath (newRow::tail)
                         
    maxPath numbers

printfn "%A" euler67