let euler49 =
    let max = 9999
    
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

    let rec combinations n list = 
        match n, list with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (head::tail) -> List.map ((@) [head]) (combinations (k-1) tail) @ combinations k tail

    let convertToString x =
        x |> List.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt32 x =
        int32 x  

    let numberPermutations number =
        let digits = number.ToString().ToCharArray() |> Array.toList
   
        permute digits
        |> List.distinct
        |> List.map convertToString
        |> List.map stringConcat
        |> List.map toInt32
        |> List.filter (fun x -> isPrime x && x > 1000)     
        |> List.sort    
    
    let result = 
        primeNumbers 
        |> Seq.map numberPermutations
        |> Seq.filter (fun x -> x |> List.length >= 3)
        |> Seq.distinct
        |> Seq.toList
        |> Seq.collect (fun list -> combinations 3 list |> List.filter (fun x -> x.[1] - x.[0] = x.[2] - x.[1]))
        |> Seq.filter (fun x -> x |> Seq.length > 0)
        |> Seq.skip 1
        |> Seq.collect (fun x -> x)
        |> Seq.map (fun x -> x.ToString())

    
    String.concat "" result

printfn "%A" euler49


