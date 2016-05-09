let euler48 =
    let numbers = [1I..1000I]
    
    let selfPower (n:bigint) =
        pown n (int32 n)
    
    let selfPowers = 
        numbers
        |> Seq.map selfPower
        |> Seq.sum
        
    let result = 
        selfPowers.ToString().ToCharArray()
        |> Seq.rev
        |> Seq.take 10
        |> Seq.rev
        |> Seq.map string
            
    String.concat "" result
    

printfn "%A" euler48