// PROBLEM 1:
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.

// Closed-form version, October 20th 2017.  Operates in O(1) time.
// I came up with this through some intuition about how to use the n*(n+1)/2 closed-form expression for the sum of [1...i].
// For multiples of 3, we're looking not at [1...i] but [3...3*i], which equals 3*[1...i], suggesting 3*i*(i+1)/2.
// But in order to make the math work out, obviously we need to adjust the value of i down from what it would be for the
// straight up sum of [1...i] - hence, we define m = i/3.  And that's the constant-time solution.
module Problem001ClosedForm =
    let sumMultiples n cutoff =
        let m = (cutoff/n)
        (n*m*(m+1))/2

    (sumMultiples 3 999) + (sumMultiples 5 999) - (sumMultiples 15 999)

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