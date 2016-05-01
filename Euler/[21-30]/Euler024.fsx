let euler24 =   
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
    
    let convertToString x =
        x |> Seq.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt64 x =
        int64 x
    
    let getNthPermutation n = 
        permutations [0..9]
        |> Seq.map convertToString
        |> Seq.map stringConcat
        |> Seq.map toInt64
        |> Seq.sort
        |> Seq.take n
        |> Seq.last
            
    getNthPermutation 1000000 

printfn "%A" euler24