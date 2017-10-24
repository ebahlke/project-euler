// PROBLEM 19:
// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

// For fun, and because I'm enamoured with Seq.unfold, done without DateTimes

module Problem019 =
    let isLeapYear n = ((n%100 <> 0) || (n%400 = 0)) && (n%4 = 0)
    let monthGaps = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31];
    
    (2, 1901)
    |>
    Seq.unfold(fun state ->
        if (snd state = 2001) then None
        else
            let februaryLength = if (isLeapYear (snd state)) then 29 else 28

            let firstOfMonths =
                (1, fst state)
                |>
                Seq.unfold(fun (month, dayOfWeek) ->
                    if (month = 13) then None
                    else
                        let monthLength = if (month = 2) then februaryLength else monthGaps.[month-1]
                        Some(dayOfWeek, (month+1, ((dayOfWeek + monthLength)%7))))
            let countOfSundays = firstOfMonths |> Seq.filter (fun day -> day = 0) |> Seq.length
            Some(countOfSundays, (((Seq.last firstOfMonths)+31)%7, snd state + 1)))
    |> Seq.sum