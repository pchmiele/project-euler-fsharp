let euler50 =
    let numbers = Seq.unfold (fun x -> Some(x, x + 1)) 2
    
    let factors n = 
        let upperBound = int32(sqrt(double n))
        {2..upperBound}
        |> Seq.filter (fun x -> n % x = 0)

    let isPrime n = 
        factors(n) 
        |> Seq.isEmpty
    
    let primeNumbers = [2..4000] |> List.filter isPrime 

    let nthPrimeNumber n = primeNumbers |> List.skip (n - 1) |> Seq.take 1 
    
    let sum numbers start finish = 
        numbers |> Seq.skip start |> Seq.take finish |> Seq.sum
        
    let result = 
        let length = primeNumbers |> Seq.length
        
        let consecutivePrimeSums = 
            seq {
                for i in 0..length do
                    for j in 0..length-i do
                        yield (sum primeNumbers i j, j)
            }
    
        consecutivePrimeSums
        |> Seq.filter (fun x -> isPrime (fst x) && fst x < 1000000)
        |> Seq.maxBy snd

    result
 
printfn "%A" euler50