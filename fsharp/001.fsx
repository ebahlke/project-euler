// PROBLEM 1:
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.

// Closed-form version, October 20th 2017.  Operates in O(1) time.
// I came up with this through some intuition about how to use the n*(n-1)/2 closed-form expression for the sum of [1...i],
// and then ad-hoc spot-checking to figure out the exact details of how to adjust the range and content of the sum itself when
// only trying to sum multiples of a certain number.  I haven't sat down to really think through or mathematically justify
// why this works - I just know it does.
module Problem001ClosedForm =
    let sumMultiplesClosedForm n cutoff =
        let m = (cutoff/n) + 1
        (n*m*(m-1))/2

    (sumMultiplesClosedForm 3 999) + (sumMultiplesClosedForm 5 999) - (sumMultiplesClosedForm 15 999)

// First version, September 2017:
module Problem001 =
    let isDivisibleByAny divisors number =
        List.exists (fun x -> number % x = 0) divisors

    let sumMultiples numbers cutoff =
        List.filter (isDivisibleByAny numbers) [1..cutoff]
        |>
        List.sum

    let result = sumMultiples [3;5] 999

// Dumb hardcoded version, also September 2017:
module Problem001Hardcoded =
    let result = List.filter (fun x -> x % 3 = 0 || x % 5 = 0) [1..999] |> List.sum