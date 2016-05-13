let euler52 =
    let isPermutation first second =
        let firstArray = first.ToString().ToCharArray() |> Array.sort
        let secondArray = second.ToString().ToCharArray() |> Array.sort

        if firstArray.Length <> secondArray.Length then false
        else Array.forall2 (fun x y -> x = y) firstArray secondArray

    let numbers = Seq.unfold (fun x -> Some(x, x + 1)) 1
    
    let result =
        let multiplers = [2..6]
    
        numbers
        |> Seq.filter (fun x -> multiplers |> Seq.forall (fun y -> isPermutation x (x*y)))
        |> Seq.head
        
    result
 
printfn "%A" euler52