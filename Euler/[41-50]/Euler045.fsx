let euler45 =  
    let isPentagonal p =
        let test = ( 1.0 + sqrt(double(24L * p + 1L))) / 6.0
        test = double(int64(test))

    let hexagonal n =
        n * (2L * n - 1L)
    
    let hexagonalNumbers = Seq.unfold (fun x -> Some(hexagonal(x), x + 1L)) 144L

    let result = 
        hexagonalNumbers
        |> Seq.filter isPentagonal
        |> Seq.head
        
    result
  
printfn "%A" euler45