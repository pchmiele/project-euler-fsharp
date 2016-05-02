let euler30 n =
    let sumOfFifthPowerOfDigits number = 
        let numberSplittedIntoDigits = 
            number.ToString().ToCharArray() 
            |> Array.map (fun x-> bigint.Parse(x.ToString()))

        Array.fold (fun acc x -> acc + (pown x n)) 0I numberSplittedIntoDigits
        
    // maximum number which can be written as sum of (digits ^ n)
    let max = bigint.Parse(n.ToString()) * (pown 9I n) 

    let result =
        Seq.unfold (fun acc -> Some(acc, acc + 1I)) 2I
        |> Seq.takeWhile (fun x -> x < max)
        |> Seq.filter (fun x -> x = sumOfFifthPowerOfDigits x)
        |> Seq.sum
                        
    result

printfn "%A" (euler30 5)