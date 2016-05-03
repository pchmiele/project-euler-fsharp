let euler31 =
    let coins = [1; 2; 5; 10; 20; 50; 100; 200]
    let max = 200
    
    let rec count n m = 
        match n with
            | n when n < 0 || m <= 0-> 0
            | n when n = 0 -> 1
            | _ -> (count n (m - 1)) + (count (n - coins.[m-1]) m)
    
    count max coins.Length
printfn "%A" euler31