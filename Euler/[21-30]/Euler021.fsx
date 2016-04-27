let euler21 =
    let divideEvenly x y =
        x % y = 0

    let divisorsOf number = 
        [1..number/2] 
        |> Seq.filter (fun x -> divideEvenly number x)
    
    let d n = divisorsOf n |> Seq.sum
    
    let result = 
        [1..9999]
        |> Seq.filter ( fun x -> x = d(d(x)) && x <> d(x) )
        |> Seq.sum
       
    result
    
printfn "%A" euler21