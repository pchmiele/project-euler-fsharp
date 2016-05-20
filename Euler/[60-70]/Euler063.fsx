let euler63 =   
    let numbers = [1.0..9.0]
       
    let result = 
        numbers
        |> Seq.map (fun x -> int32(1.0 / (1.0 - log10 x)) )
        |> Seq.sum
    result
    
printfn "%A" euler63
