let euler33 =
    let rec greatestCommonDivisor x y =
        match x with
        | 0 -> y
        | _ when y = 0 -> x
        | _ -> greatestCommonDivisor y (x%y)   
    
    let isNonTrivalExample (x, y) =
        let numeratorValueToCancel = x%10
        let denominatorValueToCancel = y/10
        if numeratorValueToCancel = denominatorValueToCancel then        
            let numerator = x/10
            let denominator = y%10
            (double(numerator)/double(denominator)) = (double(x)/double(y))
        else
            false

    let fractions = 
        let numbers = [10..99]
        numbers
        |> Seq.collect (fun x -> numbers |> Seq.map (fun y -> if x/y < 1 then Some(x, y) else None))
        |> Seq.filter (fun x -> x.IsSome)
        |> Seq.filter (fun x -> isNonTrivalExample(fst x.Value, snd x.Value))
        |> Seq.reduce (fun acc x -> Some((fst acc.Value) * (fst x.Value), (snd acc.Value) * (snd x.Value)))
        
    (snd fractions.Value) / greatestCommonDivisor (fst fractions.Value) (snd fractions.Value)

printfn "%A" euler33