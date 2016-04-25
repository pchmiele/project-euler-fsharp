let euler14 = 
    let isOdd n =
        n % 2L = 0L

    let rec collatz n acc =
        if n <= 1L then
            acc
        elif isOdd n then
            collatz (n/2L) (n::acc)
        else
            collatz (3L * n + 1L) (n::acc)

    let collatzResult n = 
        let length = collatz n [] |> Seq.length
        (n, length)
    
    let collatzSequence = Seq.unfold (fun (acc, current) -> 
        if current >= 1000000L then None
        else Some(acc, (collatzResult(current), current + 1L))) ((1L, 1), 1L)

    let result = 
        collatzSequence
        |> Seq.maxBy snd
        |> fst

    result

printfn "%A" euler14