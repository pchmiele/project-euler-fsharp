let euler49 =
    let min = 100
    let max = 1000
    
    let factors n = 
        let upperBound = int32(sqrt(double n))
        {2..upperBound}
        |> Seq.filter (fun x -> n % x = 0)

    let isPrime n = 
        factors(n) 
        |> Seq.isEmpty
    
    let primeNumbers = [2..max] |> Seq.filter (fun x -> x <= max) |> Seq.filter isPrime |> Seq.cache

    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let convertToString x =
        x |> List.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt32 x =
        int32 x  

    let numberPermutations number =
        printfn "%A===============" number
        let digits = number.ToString().ToCharArray() |> Array.toList
                        
        let permutations = 
            permute digits
            |> List.map convertToString
            |> List.map stringConcat
            |> List.map toInt32
        
        match permutations.Length with
        | 3 ->
            permutations 
            |> Seq.filter (fun x -> x <= max && primeNumbers |> Seq.contains x)
            |> Seq.sort
            |> Seq.distinct
            |> Seq.windowed 2
            |> Seq.iter (fun x -> printfn "%A" x)
        | _ ->         printfn "%A===============" number

             

    let result = 
        primeNumbers 
        |> Seq.map (fun x -> numberPermutations x ) 
        |> Seq.length
        // |> Seq.iter (fun x -> printfn "%A" x)
    
    result

printfn "%A" euler49