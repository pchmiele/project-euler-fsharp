let euler58 =  
    let factors n = 
        let upperBound = int64(sqrt(double n))
        {2L..upperBound}
        |> Seq.filter (fun x -> n % x = 0L)

    let isPrime n = 
        match n with 
        | 0L | 1L -> false
        | 2L -> true
        | _ -> factors(n) |> Seq.isEmpty

    let max = 58L
    let diagonalNumbers = 
        Seq.unfold (fun (acc, current, step) -> 
            let newStep = if current % 4L = 1L then step + 2L else step
            Some(acc, (acc + newStep, current+1L, newStep))) (1L, 1L, 0L) 

    let mutable primesCount = 0L
    let mutable diagnoalsCount = 1L
    
    let result = 
        diagonalNumbers
        |> Seq.takeWhile (fun x -> 
            if isPrime x then 
                primesCount <- (primesCount + 1L)
            diagnoalsCount <- (diagnoalsCount + 1L)
            
            match primesCount with
            | 0L -> true
            | _ -> (double primesCount)/(double diagnoalsCount) >= 0.1        
            )
        |> Seq.rev
        |> Seq.take 2
        |> Seq.reduce (-)
    
    result + 1L

printfn "%A" euler58