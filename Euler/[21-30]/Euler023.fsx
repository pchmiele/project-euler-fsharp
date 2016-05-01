let euler23 =
    let properDivisors (n:int) =
        [1..n/2]
        |> Seq.filter (fun x -> n % x = 0)
   
    let isAbundantNumber n = 
        let sumOfDivisors = (properDivisors n)|> Seq.sum
        sumOfDivisors > n

    let isNonAbundantNumber n = 
        not (isAbundantNumber n)
 
    let allAbundantNumbers =
        [1..100]
        |> Seq.filter isAbundantNumber

    let nonAbundantNumbers = 
        [1..100]
        |> Seq.filter isNonAbundantNumber

    let canBePresentedAsSumOfAbundantNumbers x = 
        let part = 1..
        
        x
        |>
    let result = 
        nonAbundantNumbers 
        |> Seq.map (fun x -> x)


    printfn "%d" (allAbundantNumbers |> Seq.length) 
    printfn "%d" (nonAbundantNumbers |> Seq.length) 

printfn "%A" euler23