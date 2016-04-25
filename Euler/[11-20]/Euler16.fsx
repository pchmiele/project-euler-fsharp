let euler16 = 
    let number = 2I ** 1000
    let result = 
        number.ToString()
        |> Seq.map (fun c -> int32(string(c)))
        |> Seq.sum

    result 

printfn "%A" euler16