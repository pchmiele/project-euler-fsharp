let euler6 = 
    let sumOfSquares =
        {1..100}
        |> Seq.map (fun x -> pown x 2)
        |> Seq.sum

    let squareOfSums =
        let sum = {1..100} |> Seq.sum
        pown sum 2

    squareOfSums - sumOfSquares
    
printfn "%d" euler6         


