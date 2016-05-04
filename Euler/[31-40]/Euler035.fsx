open System

let euler35 =  
    let hasDivisor n =
        let upperBound = int32(sqrt(double(n)))
        [2..upperBound] |> Seq.exists (fun x -> n % x = 0)
    
    let isPrime n = if n = 1 then false else not(hasDivisor(n))

    let isCircularPrime n =
        let charArray = n.ToString().ToCharArray()
        let length = charArray |> Array.length
        
        let permutations = 
            [0..(length-1)]
            |> List.map (fun x-> Array.permute (fun y -> (x + y) % length) charArray )
            |> List.map (fun x -> String.Join("", x |> Array.toList))
            |> List.map (fun x -> int32(x))

        permutations |> List.forall isPrime

    let result =
        [1..999999]
        |> Seq.filter isPrime
        |> Seq.filter isCircularPrime
        |> Seq.length
        
    result
 
printfn "%d" euler35
