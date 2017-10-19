// PROBLEM 1:
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.

module Problem001 =
    let isDivisibleByAny divisors number =
        List.exists (fun x -> number % x = 0) divisors

    let sumMultiples numbers cutoff =
        List.filter (fun x -> isDivisibleByAny numbers x) [1..cutoff]
        |>
        List.sum

    let result = sumMultiples [3;5] 999

// Dumb hardcoded version:
module Problem001Hardcoded =
    let result = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) [1..999] |> List.sum