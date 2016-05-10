let euler43 =
    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)

    let pandingitalPermutations =
        let digits = [0L..9L] |> Seq.toList
                        
        let permutations = 
            permute digits
            |> Seq.filter (fun x -> x.Length = 10)
            
        permutations
             
    let convertToString x =
        x |> Seq.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt64 x =
        int64 x  

    let firstTenPrimes = [2L; 3L; 5L; 7L; 11L; 13L; 17L]
    
    let areSubStringsDivisibleByPrimes numbers = 
        let substrings = 
            numbers 
            |> Seq.skip 1 
            |> Seq.windowed 3
            |> Seq.map (fun x -> x |> Seq.map (fun y -> y.ToString()))
            |> Seq.map (fun x -> String.concat "" x )
            |> Seq.toList
            |> Seq.map toInt64
            
        Seq.forall2 (fun x y -> x % y = 0L ) substrings firstTenPrimes
       
    let result =
        pandingitalPermutations
        |> Seq.filter areSubStringsDivisibleByPrimes
        |> Seq.map convertToString
        |> Seq.map stringConcat
        |> Seq.map toInt64
        |> Seq.sum
        
    result
 
printfn "%A" euler43