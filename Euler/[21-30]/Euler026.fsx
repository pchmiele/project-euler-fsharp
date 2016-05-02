let euler26 =
    let factors n = 
        let upperBound = int32(sqrt(float n))
        {2..upperBound}
        |> Seq.filter (fun x -> n % x = 0)

    let isPrime n = 
        if n > 1 then
            factors(n) |> Seq.isEmpty
        else
            false
  
    let formula a b n =
        pown n 2 + a * n + b
      
    let primeNumbersLength a b = 
        Seq.unfold (fun acc -> Some(acc, acc + 1)) 0
        |> Seq.takeWhile (fun x-> isPrime(formula a b x))
        |> Seq.length

    let range = [-999..999]
    let result = 
        range
        |> Seq.collect (fun a -> range |> Seq.map (fun b -> (a, b)))
        |> Seq.maxBy (fun (a, b) -> primeNumbersLength a b)
    
    fst result * snd result
    
printfn "%A" euler26