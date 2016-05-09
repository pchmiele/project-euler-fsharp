open System

let euler48 =
    let numbers = Seq.unfold (fun x -> Some(x, x + 1I)) 2I
    
    let factors n = 
        let upperBound = bigint(sqrt(double n))
        {2I..upperBound}
        |> Seq.filter (fun x -> n % x = 0I)

    let isPrime n = 
        factors(n) 
        |> Seq.isEmpty
    
    let primeNumbers = numbers |> Seq.filter isPrime |> Seq.cache
    
    let rec getPrimeFactors acc n =       
        if n = 1I then acc
        else 
            let denominator = primeNumbers |> Seq.filter (fun x -> n % x = 0I) |> Seq.head
            getPrimeFactors (acc @ [denominator]) (n/denominator) 

    let primeFactors n = getPrimeFactors []
    
    let distinctPrimeFactorsCount n = 
        (getPrimeFactors [] n) 
        |> Seq.distinct 
        |> Seq.length
        
    let result = 
        numbers
        |> Seq.windowed 4
        |> Seq.find (fun x -> x |> Seq.forall (fun y -> distinctPrimeFactorsCount y = 4)) 
        |> Seq.take 1

    result

printfn "%A" euler48