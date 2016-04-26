open System.Linq

let euler4 = 
    let isPalindrome input =
        let inputAsCharArray = input.ToString().ToCharArray()
        let reverseInputCharArray = inputAsCharArray |> Array.rev
        inputAsCharArray.SequenceEqual(reverseInputCharArray)

    let numbers = [1..999]
    
    let maxPalindromeProduct = 
        numbers
        |> Seq.collect( fun x -> numbers |> Seq.map (fun y -> x * y))
        |> Seq.filter isPalindrome
        |> Seq.max

    maxPalindromeProduct

printfn "%d" euler4         


