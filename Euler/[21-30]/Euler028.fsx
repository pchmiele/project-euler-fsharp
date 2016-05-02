let euler28 =
    let max = 1001L * 1001L

    let result = 
        Seq.unfold (fun (acc, current, step) -> 
            if acc > max then None
            else 
                let newStep = if current % 4L = 1L then step + 2L else step
                Some(acc, (acc + newStep, current+1L, newStep))) (1L, 1L, 0L) 
                
    result |> Seq.takeWhile (fun x-> x <= max ) |> Seq.sum

printfn "%A" euler28