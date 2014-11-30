source("0004.R")
source("0003.R")

greatest.prod.n.digits <- function(filename, n) {
  vector.rep <- scan(filename, sep="")
  both <- (vector.rep!=0) + (vector.rep!=1)
  current.max <- 0
  
  for (i in 1:(length(vector.rep)-(n-1))) {
    if (sum(both[i:(i+(n-1))]==2) == 13) {
      if (product(vector.rep[i:(i+(n-1))]) > current.max)
        current.max <- product(vector.rep[i:(i+(n-1))])
    }
  }
  
  current.max
}