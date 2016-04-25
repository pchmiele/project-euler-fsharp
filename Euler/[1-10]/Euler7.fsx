let euler7 = 
    let findFactorsOf n =
        let upperBound = int( sqrt( double(n) ) )
        {2..upperBound}
        |> Seq.filter (fun x -> n % x = 0)

    let isPrime n  = findFactorsOf n |> Seq.isEmpty

    let nthPrime = 
        Seq.unfold (fun x -> Some(x, x+1)) 2 
        |> Seq.filter isPrime
        |> Seq.item 10000

    nthPrime

printfn "%d" euler7