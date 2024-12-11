generate_modified_fibonacci <- function(n_terms) {
  if (n_terms <= 0) {
    return(c())
  } else if (n_terms == 1) {
    return(c(0))
  } else if (n_terms == 2) {
    return(c(0, 1))
  }
  
  series <- c(0, 1, 1)
  
  for (i in 4:n_terms) {
    next_term <- series[i - 1] + series[i - 2] + series[i - 3]
    series <- c(series, next_term)
  }
  
  return(series)
}

cat("Enter the number of terms: ")
n_terms <- as.integer(readline())

if (is.na(n_terms) || n_terms <= 0) {
  cat("Invalid input. Please enter a positive integer.\n")
} else {
  series <- generate_modified_fibonacci(n_terms)
  
  cat("The modified Fibonacci series with", n_terms, "terms is:\n", paste(series, collapse = ", "), "\n")
}
