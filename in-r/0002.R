# Author: Emma Bahlke
# Date: November 2014
# Solution to Project Euler problem #2:
# "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
# To get the solution, run sum_even_fibbonaci_numbers(4000000)

# Natural -> Natural
# Sums all even fibbonaci numbers up to some number n, assumed greater than two
sum_even_fibbonaci_numbers <- function(up_to_val) {
  latest_fibbonaci_numbers <- c(1,2)
  current_val <- 2
  current_sum <- 2
  
  while ((latest_fibbonaci_numbers[1] +
            latest_fibbonaci_numbers[2]) < up_to_val) {
    current_val <- latest_fibbonaci_numbers[1] + latest_fibbonaci_numbers[2]
    latest_fibbonaci_numbers <- rev(latest_fibbonaci_numbers)
    latest_fibbonaci_numbers[2] <- current_val
    
    if (current_val %% 2 == 0)
      current_sum <- current_sum + current_val
  }
  
  current_sum
}