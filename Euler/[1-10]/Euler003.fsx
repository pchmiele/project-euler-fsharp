let euler3 = 
    let factors (n:int64) = 
        let upperBound = int64(sqrt(float n))
        {2L..upperBound}
        |> Seq.filter (fun x -> n % x = 0L)

    let isPrime (n:int64) = 
        factors(n) 
        |> Seq.isEmpty

    let largestPrimeFactor (input:int64) = 
        factors(input)
        |> Seq.filter (fun x -> (isPrime x))
        |> Seq.last

    largestPrimeFactor 600851475143L
    
printfn "%A" euler3         
