//PROBLEM 3:
// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?

module Problem003 =
    // Attempted a Sieve of Eratosthenes-inspired version, which is sadly very non-performant after ~100000 and takes several minutes
    // to find the right answer for the given number
    let getPrimesUpTo (n:int64) =
        let listContainsNoDivisors (list:List<int64>) (a:int64) =
            not (List.exists (fun elem -> a%elem = int64(0)) list)
        List.fold (fun acc x -> if (listContainsNoDivisors acc x) then x::acc else acc) [] [int64(2)..n]

    let getLargestPrimeFactor (n:int64) =
        let potentialPrimeFactors = getPrimesUpTo n
        List.tryFind (fun elem -> n%elem = int64(0)) potentialPrimeFactors