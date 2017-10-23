// PROBLEM 12:
// What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

module Problem012 =
    let indexOfFirstFibonacciNumberOfLength n =
        (bigint(0), bigint(1))
        |>
        Seq.unfold(fun state ->
            if ((fst state).ToString().Length = n) then None
            else Some(fst state, (snd state, fst state + snd state)))
        |> Seq.length