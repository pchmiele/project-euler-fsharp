let euler1 =
    let result n =
        [1..n-1]
        |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0)
        |> Seq.sum
        
    result 1000

printfn "%d" euler1