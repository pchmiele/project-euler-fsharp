open MathNet.Numerics

let euler53 =  
    let numbers = [1..100]

    let result =
        numbers 
        |> Seq.collect (fun x -> numbers |> Seq.filter (fun y -> y <= x ) |> Seq.map (fun y -> SpecialFunctions.Binomial(x, y))) 
        |> Seq.filter (fun x -> x > 1000000.0)
        |> Seq.length 
    
    result
    
printfn "%A" euler53