let euler40 =  
    let fractionalSequence = 
         String.concat "" ([1..250000] |> Seq.map string)

    let result =     
        [1; 10; 100; 1000; 10000; 100000; 1000000]
        |> Seq.map( fun x -> fractionalSequence)
        |> Seq.map (fun x -> int32(x.ToString()))
        |> Seq.reduce (fun acc x -> acc * x)
    result     

printfn "%A" euler40