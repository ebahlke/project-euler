// PROBLEM 2:
// By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

// A helpful trick here is to note that the even terms in the Fibonacci sequence (2, 8, 34, 144...) are generated by the rule
// 4(n-1) + n-2

// I completed this one after problem 7, so I was handily able to use my new understanding of Seq.unfold :)

module Problem002 =
    let getEvenFibonacciNumbersUpTo n =
        (0, 2)
        |>
        Seq.unfold(fun state ->
            if (snd state >= n) then None
            else Some(snd state, (snd state, 4*(snd state) + fst state)))

    getEvenFibonacciNumbersUpTo 4000000 |> Seq.sum