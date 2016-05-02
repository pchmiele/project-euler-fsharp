let euler27 = 
    let rec longestCycle n = 
        match n with
            | a when a % 2I = 0I -> longestCycle (a / 2I) 
            | a when a % 5I = 0I -> longestCycle (a / 5I) 
            | _ -> 
                [1..999]
                |> Seq.filter (fun x -> ((pown 10I x) - 1I) % n = 0I)
                |> Seq.head

    let result = [1I..999I] |> Seq.maxBy longestCycle
    result

printfn "%A" euler27
