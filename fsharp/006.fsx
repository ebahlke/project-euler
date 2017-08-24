let differenceBetweenSquareOfSumAndSumOfSquares numbers =
    let square x = x*x
    let sumOfSquares = List.sumBy square numbers
    let squareOfSum = List.sum numbers |> square
    squareOfSum - sumOfSquares