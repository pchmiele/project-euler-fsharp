open System.IO

let euler11 = 
    let numbers = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler11.txt")
        File.ReadAllLines(path) 
        |> Seq.map (fun line -> line.Split(' ') |> Seq.map int32 |> Seq.toArray )
        |> Seq.toArray
    
    let height = numbers.Length
    let width = numbers |> Seq.map (fun l -> l.Length) |> Seq.max
    let twoDArray = Array2D.init height width (fun i j -> numbers.[i].[j])

    let down (array2D:int[,]) x y n =
        if y > height - n then [||]
        else array2D.[y..y+n-1, x]
     
    let right (array2D:int[,]) x y n =
        if x > width - n then [||]
        else array2D.[y, x..x+n-1]

    let rightDown (array2D:int[,]) x y n =
        if x > width - n || y > height - n then [||]
        else Array.map2 (fun y x -> array2D.[y, x]) [|y..y+n-1|] [|x..x+n-1|] 
    
    let rightUp (array2D:int[,]) x y n =
        if x > width - n || y < n - 1 then [||]
        else
            let yCollection = [|y-n+1..y|] |> Array.rev
            Array.map2 (fun y x -> array2D.[y, x]) yCollection [|x..x+n-1|]
    
    let combinations = 
        seq {
            for y in 0..height-1 do
                for x in 0..width-1 do
                    yield down twoDArray x y 4
                    yield right twoDArray x y 4
                    yield rightDown twoDArray x y 4
                    yield rightUp twoDArray x y 4
        }
    
    let product numbers = numbers |> Seq.fold (fun acc n -> (acc * n)) 1

    let result = 
        combinations
        |> Seq.map product
        |> Seq.max

    result

printfn "%d" euler11