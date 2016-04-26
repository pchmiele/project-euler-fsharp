let euler10 = 
    let factors (n:int64) = 
        let upperBound = int64(sqrt(float n))
        {2L..upperBound}
        |> Seq.filter (fun x -> n % x = 0L)

    let isPrime (n:int64) = 
        factors(n) 
        |> Seq.isEmpty

    let sumOfPrimeFactors (input:int64) = 
        [1L..input-1L]
        |> Seq.filter isPrime
        |> Seq.sum

    sumOfPrimeFactors 2000000L
    
printfn "%A" euler10         
