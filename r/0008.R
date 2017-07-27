# Author: Emma Bahlke
# Date: November 2014
# Solves Project Euler problem 8:
# "Find the thirteen adjacent digits in the 1000-digit number that have the greatest product.
# What is the value of this product?"
# The 1000-digit number is given in the problem and is saved as problem8.txt

# To solve, run greatest.prod.n.digits(problem8.txt, 13)

source("0005.R") # for "product" function

# NOTE: This function hinges on the assumption that there is a sequence of digits in the given
# number of length n or greater that contains no zeroes and ones.  The method used essentially
# discards any n-digit window that contains even a single 0 or 1, on the grounds that (if there
# exists an n-digit window containing NO zeroes or ones) this string of digits can't possibly
# result in the greatest product.
# Through a brief inspection, I manually verified that the 1000-digit number given in Project Euler
# problem 8 contains at least one sequence of 13 digits, none of which is a zero or a one.
# However, this may not be the case in general, so at the moment this method is not very generalizable.
greatest.prod.n.digits <- function(filename, n) {
  vector.rep <- scan(filename, sep="")
  both <- (vector.rep!=0) + (vector.rep!=1)
  current.max <- 0
  
  for (i in 1:(length(vector.rep)-(n-1))) {
    if (sum(both[i:(i+(n-1))]==2) == n) {
      if (product(vector.rep[i:(i+(n-1))]) > current.max)
        current.max <- product(vector.rep[i:(i+(n-1))])
    }
  }
  
  current.max
}