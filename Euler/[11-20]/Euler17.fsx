let toWord number = 
    match number with
            | 1000 -> "thousand"
            | 100 -> "hundred"
            | 90 -> "ninety"
            | 80 -> "eighty"
            | 70 -> "seventy"
            | 60 -> "sixty"
            | 50 -> "fifty"
            | 40 -> "forty"
            | 30 -> "thirty"
            | 20 -> "twenty"
            | 19 -> "nineteen"
            | 18 -> "eighteen"
            | 17 -> "seventeen"
            | 16 -> "sixteen"
            | 15 -> "fifteen"
            | 14 -> "fourteen"
            | 13 -> "thirteen"
            | 12 -> "twelve"
            | 11 -> "eleven"
            | 10 -> "ten"
            | 9 -> "nine"
            | 8 -> "eight"
            | 7 -> "seven"
            | 6 -> "six"
            | 5 -> "five"
            | 4 -> "four"
            | 3 -> "three"
            | 2 -> "two"
            | 1 -> "one"
            | _ -> null

let thousands number = 
    let thousandsCount = number / 1000 
    match thousandsCount with
        | x when x > 0 && x < 10 -> toWord(thousandsCount) + " " + toWord(1000)
        | _ -> ""

let hundreds number = 
    let result = []
    let hundredsCount = number / 100 
    match hundredsCount with
        | x when x > 0 && x < 10 -> toWord(hundredsCount) + " " + toWord(100)
        | _ -> ""

let dozens number =
    let result = []
    let dozens = number % 100
    match dozens with
        | a when a > 0 && a < 20 -> toWord(dozens)
        | a when a >= 20 && a < 100 -> toWord(dozens - dozens % 10) + " " + toWord(dozens % 10)
        | _ -> ""

let numberAsWord number =  
    let thousands = thousands number
    let hundreds = hundreds number
    let dozens = dozens number
    let conjunction = if dozens = "" then "" else "and"

    let result = (thousands +  hundreds + conjunction + dozens).Replace(" ", "")
    printfn "%s" result
    result
    
let result = 
    [1..1000] 
    |> Seq.map numberAsWord 
    |> Seq.sumBy(fun x -> x.Length)

printfn "%d" result