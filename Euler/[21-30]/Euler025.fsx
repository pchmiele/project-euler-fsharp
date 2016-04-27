let euler25 =
    let fibbonacciSeq = Seq.unfold (fun (acc, x) -> Some(acc, (x, acc + x))) (0I,1I)
    let lessThan1000Characters x =
        let asString = string x
        asString.Length < 1000
 
    let result = 
        fibbonacciSeq
        |> Seq.takeWhile lessThan1000Characters
        |> Seq.length        
       
    result
    
printfn "%d" euler25