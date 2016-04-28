open System.IO

let euler22 =
    let alphabeticalValue (name:string) = 
        name.ToLower().ToCharArray() 
        |> Seq.map (fun l -> ( int32(l) - 96))
        |> Seq.sum
     
    let nameScore (index:int, name:string) =
        (index+1) * alphabeticalValue(name)
    
    let normalizeWord (word:string) = 
        word.ToLower().Replace("\"", "")
        
    let names = 
        let path = Path.Combine(__SOURCE_DIRECTORY__,"..","data/euler22.txt")
        File.ReadAllLines(path) 
        |> Seq.map (fun line -> line.Split(','))
        |> Seq.concat
        |> Seq.map normalizeWord
        |> Seq.sort
        |> Seq.indexed 
        |> Seq.map nameScore
        |> Seq.sum
        
    names

printfn "%d" euler22