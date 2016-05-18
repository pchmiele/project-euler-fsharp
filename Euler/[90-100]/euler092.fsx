open System.Collections.Generic

let euler92 =
    let numbers = [1..9999999]
    let map = dict
    let numberAsListOfDigits number = 
        number.ToString().ToCharArray()
        |> Seq.map (fun x -> int32(x.ToString()))

    let squareAllmembers collection =
        collection |> Seq.map (fun x -> pown x 2)

    let numberAsSumOfSureDigits number = 
        numberAsListOfDigits number 
        |> squareAllmembers
        |> Seq.sum
    
    let rec squareDigitChainArriveAt89 (chain:Dictionary<int32,bool>) number =       
        match number with
        | 1 -> false
        | number when chain.ContainsKey(number) -> true
        | _ -> squareDigitChainArriveAt89 chain (numberAsSumOfSureDigits number)

    let rec squareDigitChains numbers (acc:Dictionary<int32,bool>) = 
        match numbers with
        | [] -> acc
        | [head] ->
            let arriveAt89 = squareDigitChainArriveAt89 acc head
            match arriveAt89 with
            | true -> 
                acc.Add(head, true)
                squareDigitChains [] acc 
            | false -> squareDigitChains [] acc
        | head::tail ->
            let arriveAt89 = squareDigitChainArriveAt89 acc head
            match arriveAt89 with
            | true -> 
                acc.Add(head, true)
                squareDigitChains tail acc
            | false -> squareDigitChains tail acc 

    let dict = new Dictionary<int32, bool>()
    let result = squareDigitChains numbers dict |> Seq.length
    
    result 

printfn "%A" euler92
