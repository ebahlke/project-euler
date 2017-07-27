# Author: Emma Bahlke
# Date: Novemer 2014
# Solves Project Euler problem 9:
# "A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# a^2 + b^2 = c^2
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc."

# To solve, run pythagorean.triplet.prod(1000)

source("0003.R") # for "is.wholenumber" function
source("0005.R") # for "product" function

# This method saves a minor amount of computation through one simple observation:
# We don't need the right triangle whose perimeter is 1000, only the first triangle we find
# which is similar to that triangle (i.e. has the same angles, i.e. a + b + c = 1000/k).  Then
# we can simply return the product of ka + kb + kc.
# In the case where triplet.sums.to = 1000, it cuts the system time from 0.194 seconds down to
# 0.009.
pythagorean.triplet.prod <- function(triplet.sums.to) {
  for (a in 2:(triplet.sums.to/2)) {
    b <- a
    while ((a + b + sqrt(a^2 + b^2)) <= triplet.sums.to) {
      
      if (triplet.sums.to %% (a + b + sqrt(a^2 + b^2)) == 0) {
        triplet <- c(a, b, sqrt(a^2 + b^2))
        if (sum(triplet) == triplet.sums.to) return(product(triplet))
        else return(product((triplet.sums.to/sum(triplet))*triplet))
      }
      
      else b <- b+1
    }
  }
  
  print("No triplet found.")
}