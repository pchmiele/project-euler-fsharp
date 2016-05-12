let euler56 =  
    let numbers = [1..99]
    
    let digitsSum n =
        n.ToString().ToCharArray() 
        |> Array.toList
        |> List.map (fun x -> (bigint.Parse(string x)))
        |> List.sum
    
    let result = 
        numbers 
        |> Seq.collect(fun x -> numbers |> Seq.map (fun y -> pown (bigint x) (y)))
        |> Seq.map digitsSum
        |> Seq.max
  
    result

printfn "%A" euler56
