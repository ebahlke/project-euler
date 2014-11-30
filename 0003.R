# integer -> numeric vector
# returns a vector containing all prime factors of a number
get.prime.factors <- function(n) {
  if ((n == 0) || (abs(n) == 1))
    c()
  else if ((abs(n) == 2) || (abs(n) == 3))
    abs(n)
  else {
    cutoff <- floor(sqrt(abs(n)))
    
    for (i in 2:cutoff) {
      if (is.wholenumber(n/i))
        return (c(i, get.prime.factors(n/i)))
    }
    
    abs(n)
  }
}

# number -> boolean
# checks whether or not a number is a whole (integer) number
is.wholenumber <- function(n) {
  ((n - floor(n)) == 0)
}

# numeric vector -> number
# returns the lowest common multiple of a list of numbers
lowest.common.multiple <- function(factors) {
  if (length(factors) == 0)
    0
  else {
    all.prime.factors <- list()
    reduced.prime.factors <- c()
    
    for (i in 1:length(factors))
      all.prime.factors <- append(all.prime.factors, list(get.prime.factors(factors[i])))
    
    distinct.factors <- get.distinct.prime.factors(factors)
    
    for (j in 1:length(distinct.factors))
      reduced.prime.factors <- c(reduced.prime.factors,
                                 rep(distinct.factors[j], count.max.occurrence(all.prime.factors,
                                                                               distinct.factors[j])))
    
    product(reduced.prime.factors)
  }
}

# numeric vector -> numeric vector
# returns the reduced (i.e. each prime factor appears only once)
# list of distinct prime factors shared among a set of numbers
get.distinct.prime.factors <- function(list) {
  if (length(list) == 0)
    c()
  else {
    all.prime.factors <- c()
    distinct.factors <- c()
    
    for (i in 1:length(list)) {
      all.prime.factors <- c(all.prime.factors, get.prime.factors(list[i]))
    }
    
    while (length(all.prime.factors) > 0) {
      next.distinct.factor <- all.prime.factors[1]
      distinct.factors <- c(distinct.factors, next.distinct.factor)
      all.prime.factors <- all.prime.factors[all.prime.factors!=next.distinct.factor]
    }
    
    sort(distinct.factors)
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