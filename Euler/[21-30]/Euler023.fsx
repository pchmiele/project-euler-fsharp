let euler23 =
    let max = 28123
    
    let properDivisors (n:int) =
        [1..n/2]
        |> List.filter (fun x -> n % x = 0)
   
    let isAbundantNumber n = 
        let sumOfDivisors = (properDivisors n)|> List.sum
        sumOfDivisors > n
 
    let allAbundantNumbers =
        [1..max] |> List.filter isAbundantNumber

    let allSumsOf2AbundantNumbers = 
        allAbundantNumbers
        |> List.collect (fun x -> allAbundantNumbers |> List.map (fun y -> x + y))
        |> List.filter (fun x -> x <= max)
        |> List.distinct
        
    let result = 
        let sumOfNumbers = [1..max] |> List.sum
        let sumOfSumsOf2AbundatNumbers = allSumsOf2AbundantNumbers |> List.sum

        sumOfNumbers - sumOfSumsOf2AbundatNumbers
        
    result

printfn "%d" euler23