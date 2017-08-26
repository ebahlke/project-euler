//PROBLEM 3:
// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?

module Problem003 =
    // Attempted a Sieve of Eratosthenes-inspired version, which is sadly very non-performant after ~100000 and takes several minutes
    // to find the right answer for the number necessary (floor of sqrt 600851475143.0)
    let getPrimesUpTo n =
        let listContainsNoDivisors list a =
            not (List.exists (fun elem -> a%elem = 0) list)
        List.fold (fun acc x -> if (listContainsNoDivisors acc x) then x::acc else acc) [] [2..n]

    let getLargestPrimeFactor' (n:float) =
        let potentialPrimeFactors = getPrimesUpTo ((sqrt >> int) n)
        List.tryFind (fun elem -> n%(float elem) = 0.0) potentialPrimeFactors

    // isPrime function taken from http://www.fssnip.net/3X
    // This version is considerably more performant.
    let isPrime n =
        let sqrt' = (float >> sqrt >> int) n
        [ 2 .. sqrt' ]
        |> List.forall (fun x -> n % x <> 0)

    let getLargestPrimeFactor (n:float) =
        let cutoff = (sqrt >> int) n
        List.rev [1..cutoff] |> List.tryFind (fun elem -> isPrime elem && n%(float elem) = 0.0)