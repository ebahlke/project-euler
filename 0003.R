# Author: Emma Bahlke
# Date: November 2014
# Solution to Project Euler problem #3:
# "What is the largest prime factor of the number 600851475143?"

# integer -> numeric vector
# returns a vector containing all prime factors of a number
get.prime.factors <- function(n) {
  prime.factors <- c()
  
  if ((n == 0) || (abs(n) == 1))
    prime.factors
  else {
    while (n %% 2 == 0) {
      prime.factors <- append(prime.factors, 2)
      n <- n/2
    }
    
    # if it's an even number, just stop here:
    if (n == 1)
      return (prime.factors)
    
    # otherwise, check for odd factors:
    cutoff <- floor(sqrt(abs(n)))
    potential.factor = 3
    
    while (potential.factor <= cutoff) {
      if (is.wholenumber(n/potential.factor))
        return (c(prime.factors, potential.factor, get.prime.factors(n/potential.factor)))
      else potential.factor <- potential.factor + 2
    }
    
    # in the case where there are no other odd factors, the remaining value of n is prime:
    c(prime.factors, abs(n))
  }
}
    
# number -> boolean
# checks whether or not a number is a whole (integer) number
is.wholenumber <- function(n) {
  ((n - floor(n)) == 0)
}