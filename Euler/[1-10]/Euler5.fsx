let euler5 = 
    let isEvenlyDivided dividend divisor = 
        dividend % divisor = 0

    let isEvenlyDividedByAll n numbers =
        numbers |> Seq.forall (fun x -> isEvenlyDivided n x)

    let findSmallestCommonMultiple numbers =
        let max = numbers |> Array.max

        Seq.unfold (fun acc -> Some(acc, acc + 1)) max
        |> Seq.filter (fun x -> isEvenlyDividedByAll x numbers)
        |> Seq.head

    findSmallestCommonMultiple [|1..20|]

printfn "%d" euler5