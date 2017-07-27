# Author: Emma Bahlke
# Date: November 2014
# Solves Project Euler problem 5:
# "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
# To solve, run lowest.common.multiple(1:20)

source("0003.R") # for "get.prime.factors" method

# numeric vector -> number
# returns the lowest common multiple of a list of numbers
lowest.common.multiple <- function(factors) {
  if (length(factors) == 0)
    0
  else {
    all.prime.factors <- list() # will be a list of LISTS representing the prime factorization of each number
    reduced.prime.factors <- c() # will be a list containing the least combination of primes from each number's prime factorization
    
    for (i in 1:length(factors))
      all.prime.factors <- append(all.prime.factors, list(get.prime.factors(factors[i])))
    
    flattened.factorizations <- unlist(all.prime.factors)
    
    while (length(flattened.factorizations) > 0) {
      next.factor <- flattened.factorizations[1]
      
      reduced.prime.factors <- c(reduced.prime.factors,
                                 rep(next.factor, count.max.occurrence(all.prime.factors,
                                                                       next.factor)))
      
      flattened.factorizations <- flattened.factorizations[flattened.factorizations != next.factor]
    }
    
    product(reduced.prime.factors)
  }
}

# (list of numeric vectors) number -> natural
# returns the maximum number of times the given number occurs in any of the vectors in the list
count.max.occurrence <- function(list.of.vectors, number) {
  if (length(list.of.vectors) == 0)
    0
  else {
    max <- 0
    
    for (i in 1:length(list.of.vectors)) {
      if ((sum(list.of.vectors[[i]]==number)) > max) # uses the fact that in the sum function,
        # TRUE = 1 and FALSE = 0
        max <- sum(list.of.vectors[[i]]==number)
    }
    
    max
  }
}

# numeric vector -> number
# multiplies all entries in a list together
product <- function(vector) {
  if (length(vector) == 0)
    0
  else {
    so.far <- 1
    for (i in 1:length(vector))
      so.far <- so.far * vector[i]
    
    so.far
  }
}