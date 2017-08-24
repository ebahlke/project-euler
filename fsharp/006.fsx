// PROBLEM 6:
// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

// Version 1 takes any list of numbers and operates in O(n) time
module Problem006 =
    let differenceBetweenSquareOfSumAndSumOfSquares numbers =
        let square x = x*x
        let sumOfSquares = List.sumBy square numbers
        let squareOfSum = List.sum numbers |> square
        squareOfSum - sumOfSquares

// Version 2 only takes a range from 1..n but operates in O(1) time using closed-form expressions
module Problem006ClosedForm =
    let differenceBetweenSquareOfSumAndSumOfSquaresUpTo n =
        ((n*(n+1))/2 * (n*(n+1))/2) - (n*(n+1)*(2*n+1))/6