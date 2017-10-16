// PROBLEM 7:
// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?
// Decided to have some fun with sequences here.  Haven't really thought through performance, but it "only" takes 9-10 seconds to
// get the correct answer for PE007, which is at least semi-reasonable.

module Problem007 =
    let getListOfNPrimes n =
        if (n <= 1) then [2]
        else
            ([2], 3)
            |> Seq.unfold(fun (primesSoFar, nextNumberToCheck) ->
                if (primesSoFar.Length = n) then None
                else if not (List.exists (fun p -> nextNumberToCheck%p = 0) primesSoFar) then
                    Some(nextNumberToCheck::primesSoFar, (nextNumberToCheck::primesSoFar, nextNumberToCheck + 1))
                else Some(primesSoFar, (primesSoFar, nextNumberToCheck + 1)))
            |> Seq.last