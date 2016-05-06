let euler32 =
    let getPossibleMultiplicantAndMultiplerCombinations number = 
        [1..int32(sqrt(double(number)))]
        |> List.filter (fun x -> number % x = 0)
        |> List.map (fun x -> 
                        let result =  number/x
                        match x with
                        | x when result < number/result -> (result, number/result, number)
                        | _ -> (number/result, result, number))
        |> List.distinct
        
    let IsPandingital (multiplicant, multipler, number) =
        let array = (multiplicant.ToString() + multipler.ToString() + number.ToString()).ToCharArray()
        match array.Length with
        | 9 when not (array |> Array.contains '0') ->  
            let arrayWithoutDuplicates = array |> Array.distinct
            array.Length = arrayWithoutDuplicates.Length
        | _ -> false
       
    let result =
        [1234..9867]
        |> Seq.map getPossibleMultiplicantAndMultiplerCombinations
        |> Seq.collect (fun x -> x |> Seq.filter IsPandingital) 
        |> Seq.map (fun (x,y,product) -> product)
        |> Seq.distinct
        |> Seq.sum      
    result
 
printfn "%A" euler32