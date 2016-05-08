open System.IO

let euler42 =
    let alphabeticalValue (name:string) = 
        name.ToLower().ToCharArray() 
        |> Seq.map (fun l -> ( int32(l) - 96))
        |> Seq.sum
    
    let isPentagonal p =
        let test = (sqrt(double(8 * p + 1)) - 1.0) / 2.0
        test = double(int64(test))

    let nameScore (name:string) =
        alphabeticalValue(name)
    
    let normalizeWord (word:string) = 
        word.ToLower().Replace("\"", "")
        
    let names = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler42.txt")
        File.ReadAllLines(path) 
        |> Seq.map (fun line -> line.Split(','))
        |> Seq.concat
        |> Seq.map normalizeWord
        |> Seq.sort
        // |> Seq.iter (fun x -> printfn "%A" x)
        |> Seq.map nameScore
        |> Seq.filter isPentagonal
        |> Seq.length
        
    names

printfn "%A" euler42