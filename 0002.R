fibbonaci <- function(max_val) {
  fibbonaci_numbers <- c(1,2)
  current_index <- 2
  current_val <- 2
  
  while ((fibbonaci_numbers[current_index] +
            fibbonaci_numbers[current_index - 1]) < max_val) {
    current_val <- fibbonaci_numbers[current_index] + fibbonaci_numbers[current_index - 1]
    fibbonaci_numbers <- c(fibbonaci_numbers, current_val)
    current_index <- current_index + 1
  }
  
  fibbonaci_numbers
}

get_solution <- function() {
  fibbonaci_numbers <- fibbonaci(4000000)
  selector <- seq(2,length(fibbonaci_numbers), 3)
  
  sum(fibbonaci_numbers[selector])
}