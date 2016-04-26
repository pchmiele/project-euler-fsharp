let euler20 =
    let rec factorial (n:bigint) =
        if n <= 1I then 1I
        else n * factorial(n - 1I)
    
    let result = 
        factorial 100I
        |> string
        |> Seq.toArray
        |> Seq.map (fun x -> int32(x.ToString()))
        |> Seq.sum
    result

printfn "%d" euler20