source("0003.R")

pythagorean.triplet.prod <- function(triplet.sums.to) {
  for (a in 2:triplet.sums.to) {
    b <- a
    while ((a + b + sqrt(a^2 + b^2)) <= triplet.sums.to) {
      
      if (is.wholenumber(sqrt(a^2+b^2)) && ((triplet.sums.to %% (a + b + sqrt(a^2 + b^2))) == 0)) {
        triplet <- c(a, b, sqrt(a^2 + b^2))
        if (sum(triplet) == triplet.sums.to) return(product(triplet))
        else return(product((triplet.sums.to/sum(triplet))*triplet))
      }
      
      else b <- b+1
    }
  }
  
  print("No triplet found.")
}