open System.Linq

let euler36 =  
    let rec toByte number = 
        match number with
        | 0 -> ""
        | _ -> (toByte (number/2)).ToString() + (number%2).ToString()
    
    let isPalindrome input =
        let inputAsCharArray = input.ToString().ToCharArray()
        let reverseInputCharArray = inputAsCharArray |> Array.rev
        inputAsCharArray.SequenceEqual(reverseInputCharArray)

    let numbers = [1..999999]
    
    let maxPalindromeProduct = 
        numbers
        |> Seq.filter isPalindrome
        |> Seq.filter (fun x -> isPalindrome(toByte))
        |> Seq.sum

    maxPalindromeProduct

printfn "%d" euler36
