library(foreach)

solve <- function(num.digits) {
   upper.bound <- (10^num.digits) - 1
   lower.bound <- 10^(num.digits - 1)
   range <- upper.bound:lower.bound
   
   mat.1 <- matrix(range, length(range), 1)
   mat.2 <- matrix(range, 1, length(range))
   all.multiples <- mat.1 %*% mat.2
   #remove.duplicates <- get.upper.diag.entries(all.multiples)
  
  find.largest.palindrome(as.vector(all.multiples))
}

# vector -> number | FALSE
# finds the largest palindrome number in a given list of numbers
find.largest.palindrome <- function(list) {
  to.check <- sort(list, decreasing=TRUE)
  for (i in to.check) {
    if (is.palindrome(i))
      return (i)
  }
  
  print("No palindromes found in this list.")
  FALSE
}

is.palindrome <- function(n) {
  return (n == reverse(n))
}

reverse <- function(number) {
  vector <- convert.to.vector(number)
  convert.to.number(rev(vector))
}

# converts a number to a vector version of its base-10 representation
convert.to.vector <- function(number) {
  if (number < 10)
    c(number)
  else {
    ones.value <- number%%10
    c(convert.to.vector(floor(number/10)), ones.value) # floor(number/10) gives the "remainder" of the number
                                                       # once the ones value has been removed, reduced approriately
  }
}

# converts a vector designating the base-10 representation of a number
# (i.e. highest place value first - c(3, 7) would be 37) into a single numeric value
convert.to.number <- function(vector) {
  sum <- 0
  for (i in 1:length(vector))
    sum <- sum + ((rev(vector))[i]*(10^(i-1))) # i.e. for the vector (a_0, a_1, ..., a_k),
                                               # run through a_k x 10^0 + ... + a_0 x 10^k
  sum
}

# m x m matrix -> vector
# returns the entries of the "upper diagonal" portion of a matrix as a vector
get.upper.diag.entries <- function(matrix) {
  to.return <- c()
  
  to.return <- unlist(foreach(i=1:nrow(matrix)) %do% matrix[i, i:nrow(matrix)])
  # using unlist here instead of .combine='c'
  # because it leads to a fivefold reduction in evaluation time
  
  to.return
}