let euler37 =
    let hasDividers n = 
        [2..int32(sqrt(double n))]
        |> Seq.exists (fun x -> n % x = 0)

    let isPrime n = 
        if n = 1 then false else not(hasDividers(n)) 
    
    let rec rightVariants (charList:List<char>) =
        match charList with
        | head::tail -> [charList] @ (rightVariants tail) 
        | head when head <> [] -> [head] 
        | _ -> []
    
    let rec leftVariants (charList:List<char>) =
        let length = charList |> List.length
        
        match length with
        | 1 -> [charList]
        | _ -> [charList] @ leftVariants (charList |> List.take (length - 1)) 
    
    let variants number =
        let charList = number.ToString().ToCharArray() |> Array.toList
    
        (leftVariants charList) @ (rightVariants charList) 
        |> List.distinct
        |> List.map (fun x -> x |> List.map string)
        |> List.map (fun x -> int32(String.concat "" x))
    
    let isTruncatablePrime n =
        variants n |> List.forall isPrime
    
    let numbers = 
        Seq.unfold (fun x -> Some(x, x+1)) 8
       
    let result =
        numbers
        |> Seq.filter isPrime
        |> Seq.filter isTruncatablePrime
        |> Seq.take 11
        |> Seq.sum

    result
 
printfn "%A" euler37