open System

let euler38 =       
    let IsPandingital number =
        let array = number.ToString().ToCharArray()
        match array.Length with
        | 9 when not (array |> Array.contains '0') ->  
            let arrayWithoutDuplicates = array |> Array.distinct
            array.Length = arrayWithoutDuplicates.Length
        | _ -> false
    

    let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

    let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
         
    let convertToString x =
        x |> Seq.map (fun y -> (string y))
    
    let stringConcat x =
        String.concat "" x
    
    let toInt32 x =
        int32 x
    
    let take (list:List<char>) position count = 
        let elements = 
            list 
            |> List.skip position 
            |> List.take count 
            |> List.map (fun x -> string x)
        
        int32(String.concat "" elements)

    let firstVariant digits = 
        let first = int32((digits |> List.head).ToString())
        let second = take digits 1 2
        let third = take digits 3 2
        let fourth = take digits 5 2
        let fifth = take digits 7 2
        
        // printfn "%A %A %A %A %A" first second third fourth fifth
        
        let result = (first = second/2 && first = third/3 && first = fourth/4 && first = fifth/5)        
        result

    let secondVariant digits = 
        let first = take digits 0 3
        let second = take digits 3 3
        let third = take digits 6 3
        
        // printfn "%A %A %A" first second third 
        
        let result = (first = second/2 && first = third/3)        
        result

    let thirdVariant digits = 
        let first = take digits 0 4
        let second = take digits 4 5
        
        // printfn "%A %A" first second 
        
        let result = (first = second/2)        
        result

    let IsPandingitalMultiple number =
        let digits = number.ToString().ToCharArray() |> Array.toList        
        let result = (firstVariant digits) || (secondVariant digits) || (thirdVariant digits)    
        result
        
    let pandingalNumberPermutations n =
        let digitList = n.ToString().ToCharArray() |> Array.toList
                
        let permutations = 
            permute digitList
            |> List.map convertToString
            |> List.map stringConcat
            |> List.map toInt32
            |> List.sort
            |> List.rev
            
        permutations

    let result =
        pandingalNumberPermutations 123456789
        |> Seq.filter IsPandingitalMultiple
        |> Seq.head

    result
 
printfn "%A" euler38
