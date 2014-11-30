sum.vs.squares <- function(numbers) {
  squared.sum <- (sum(numbers))^2
  summed.squares <- sum(apply(array(numbers), 1, sqr))
  
  squared.sum - summed.squares
}

sqr <- function(n) {
  n^2
}