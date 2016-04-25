open MathNet.Numerics

let euler15 = 
    let result = SpecialFunctions.Binomial(40, 20)
    bigint result
    
printfn "%A" euler15
