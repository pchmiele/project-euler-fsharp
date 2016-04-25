let euler9 = 
    let triples = 
        seq {
            for a = 1 to 1000 do
                for b = 1 to 1000 do 
                    for c = 1 to 1000 do
                    if a + b + c = 1000 then yield [a;b;c]
        }

    let isPythagoreanTriplet (numbers: int list) =
        match List.sort(numbers) with
        | [a;b;c] -> a*a + b*b = c*c
        | _ -> false

    let pythagoreanTriplet = triples |> Seq.filter isPythagoreanTriplet |> Seq.head
    let product = pythagoreanTriplet |> Seq.fold (fun acc n -> (acc * n) ) 1
    product

printfn "%d" euler9         