# Author: Emma Bahlke
# Date: November 2014
# Solves Project Euler problem 6:
# "Find the difference between the sum of the squares
# of the first one hundred natural numbers and the square of the sum."

# To solve, run sum.vs.squares(1:100)

sum.vs.squares <- function(numbers) {
  squared.sum <- (sum(numbers))^2
  summed.squares <- sum(apply(array(numbers), 1, sqr))
  
  squared.sum - summed.squares
}

sqr <- function(n) {
  n^2
}