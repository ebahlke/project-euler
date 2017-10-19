// PROBLEM 7:
// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
// What is the 10 001st prime number?

// Decided to have some fun with sequences here.  Haven't really thought through performance, but it "only" takes 9-10 seconds to
// get the correct answer for PE007, which is at least semi-reasonable.  However, there are a lot of inefficiencies with the algorithm.
// So it's not ideal - I haven't figured out yet how to use sequences to "skip"/continue rather than stopping -
// the ideal version would be something like:
// Seq.unfold(fun (primesSoFar, nextNumberToCheck) ->
//  if not any primesSoFar divide nextNumberToCheck then Some(nextNumberToCheck, (nextNumberToCheck::primesSoFar, nextNumberToCheck+1))
//  else just continue
// (for an infinite list - could then be piped into Seq.take n)
// but unfortunately I can't figure out a way not to stop/yield None in the "else" statement.
// See also my struggles with getting a performant version in problem 003
// TODO: Find a good, optimized algorithm for generating a list of n primes, or primes up to n, in F#

module Problem007 =
    let getListOfNPrimes n =
        if (n <= 1) then [2]
        else
            ([2], 3)
            |> Seq.unfold(fun (primesSoFar, nextNumberToCheck) ->
                if (primesSoFar.Length = n) then None
                else if not (List.exists (fun p -> p*p <= nextNumberToCheck && nextNumberToCheck%p = 0) primesSoFar) then
                    let nextPrimes = nextNumberToCheck::primesSoFar
                    Some(nextPrimes, (nextPrimes, nextNumberToCheck + 1))
                else Some(primesSoFar, (primesSoFar, nextNumberToCheck + 1)))
            |> Seq.last