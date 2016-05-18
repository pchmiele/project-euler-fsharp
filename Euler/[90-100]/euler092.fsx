open System.Collections.Generic

let euler92 =
    let max = 9999999
    let numbers = [1..max]

    let numberAsListOfDigits number = 
        number.ToString().ToCharArray()
        |> Seq.map (fun x -> int32(x.ToString()))

    let squareAllmembers collection =
        collection |> Seq.map (fun x -> pown x 2)

    let numberAsSumOfSureDigits number = 
        numberAsListOfDigits number 
        |> squareAllmembers
        |> Seq.sum
    
    let rec squareDigitChainArriveAt89 (cache:bool option array) (chain:list<int32>) number =       
        let isCached = cache.[number]
        match isCached with
        | Some value -> 
                chain |> List.iter (fun x -> cache.[x] <- Some(value))
                value
        | None -> squareDigitChainArriveAt89 cache (chain @ [number]) (numberAsSumOfSureDigits number)

    let cache = Array.init ((numberAsSumOfSureDigits max) + 1) (fun n -> match n with | 1 -> Some(false) | 89 -> Some(true) | _ -> None)
    let result = 
        numbers
        |> Seq.filter (fun x -> squareDigitChainArriveAt89 cache [] (numberAsSumOfSureDigits x))
        |> Seq.length
    
    result 

printfn "%A" euler92