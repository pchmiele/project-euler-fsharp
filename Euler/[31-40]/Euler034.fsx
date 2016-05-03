let euler34 =
    // According to wikipedia this is upper bound for six digit factorion
    let upperBound = 1854721
    
    let factorials = 
        Array.unfold (fun (acc, x) -> 
            match x with
                | x when x <= 10 -> Some(acc, (acc * x, x + 1)) 
                | _ -> None)  (1, 1)
      
    let isFactorion number = 
        let numberSplittedIntoDigits = 
            number.ToString().ToCharArray() 
            |> Array.map (fun x-> int32(x.ToString()))

        let sumOfFactorialOfDigits = Array.fold (fun acc x -> acc + (factorials.[x])) 0 numberSplittedIntoDigits
        number = sumOfFactorialOfDigits
        
    let result =
        [3..upperBound]
        |> Seq.filter isFactorion
        |> Seq.sum
                        
    result

printfn "%A" euler34