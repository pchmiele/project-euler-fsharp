let euler19 =   
    let startDate = System.DateTime.Parse "1-1-1901"
    let endDate = System.DateTime.Parse "12-31-2000"

    let months = 
        Seq.unfold (fun currentDate -> 
            if currentDate >= endDate then 
                None 
            else
                let nextMonth = currentDate.AddMonths 1
                Some(currentDate, nextMonth)
            ) (startDate)
            
    let isSunday (day:System.DateTime) = 
        day.DayOfWeek = System.DayOfWeek.Sunday

    let sundaysCount = 
        months
        |> Seq.filter isSunday
        |> Seq.length

    sundaysCount
    
printfn "%d" euler19