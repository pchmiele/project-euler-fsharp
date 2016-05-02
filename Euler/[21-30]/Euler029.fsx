let euler29 =
    let distinctPowers (a:int32) (b:int32) =
       let scope = [a..b] 
    
       scope
       |> List.collect (fun x-> scope |> List.map (fun y -> (pown (bigint x) y)))
       |> List.distinct 
       |> List.length
           
    distinctPowers 2 100

printfn "%A" euler29