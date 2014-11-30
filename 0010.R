source("0003.R")

primes.below.n <- function(n) {
  primes.so.far <- c(2)
  
  for (i in seq(3, n, 2)) {
    if (length(get.prime.factors(i)) == 1)
      primes.so.far <- append(primes.so.far, i)
  }
  
  primes.so.far
}