let euler39 =
    let numberOfSolutions p =
        [2..p/3]
        |> Seq.filter (fun x -> p * (p-2*x) % (2*(p - x)) = 0)
        |> Seq.length
    
    let result =
        [1..1000]
        |> Seq.mapi (fun index x -> (index, numberOfSolutions x))
        |> Seq.maxBy (fun (index, x) -> x)
        
    (fst result) + 1
printfn "%A" euler39