let euler41 =
    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
         
    let convertToString x =
        x |> Seq.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt32 x =
        int32 x
        
    let pandingitalNumberPermutations n =
        let digits = [1..n] |> Seq.map (fun x -> char(x.ToString())) |> Seq.toList
                        
        let permutations = 
            permute digits
            |> Seq.map convertToString
            |> Seq.map stringConcat
            |> Seq.map toInt32
            
        permutations
    
    let factors n = 
        let upperBound = int32(sqrt(double n))
        {2..upperBound}
        |> Seq.filter (fun x -> n % x = 0)

    let isPrime n = 
        factors(n) 
        |> Seq.isEmpty
    
    let firstTenPrimes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 31]
    
    let result =
        [1..9]
        |> Seq.collect (fun x -> pandingitalNumberPermutations x)
        |> Seq.filter (fun x -> not(firstTenPrimes |> Seq.exists (fun y -> x % y = 0)))
        |> Seq.sort
        |> Seq.rev
        |> Seq.filter isPrime 
        |> Seq.head
        
    result
 
printfn "%A" euler41