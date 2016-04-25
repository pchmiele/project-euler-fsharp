let euler12 = 
    let triangles = 
        Seq.unfold (fun (acc, current) -> Some(acc, (acc + current, current + 1))) (0, 1) 
        |> Seq.skip 1

    let rec allFactors dividend divisor factors =
        if dividend % divisor = 0 then
            let x = dividend / divisor
            if divisor < x then
                allFactors dividend (divisor + 1) (divisor::x::factors)
            elif divisor = x then
                divisor::factors
            else
                factors 
        elif divisor > int(sqrt(float dividend)) then
            factors 
        else
            allFactors dividend (divisor + 1) factors

    let factorsCount n = 
        let count = allFactors n 1 [] |> Seq.length
        (n, count)
    
    let result = 
        triangles 
        |> Seq.map factorsCount 
        |> Seq.find (fun x -> snd x > 500)
        |> fst

    result

printfn "%d" euler12