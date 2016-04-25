let euler2 =
    let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0, 1)

    let result = fibonacciSeq 
                |> Seq.takeWhile (fun x -> x < 4000000)
                |> Seq.filter (fun x -> x % 2 = 0)
                |> Seq.sum

    result

printfn "%d" euler2

