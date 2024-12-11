calculate_series_sum <- function(n_terms) {
  sum <- 0
  sign <- 1
  
  for (i in 1:n_terms) {
    term <- sign * (i / (2 * i - 1))
    sum <- sum + term
    sign <- -sign  
  }
  
  return(sum)
}

cat("Enter the number of terms: ")
n_terms <- as.integer(readline())

if (is.na(n_terms) || n_terms <= 0) {
  cat("Invalid input. Please enter a positive integer.\n")
} else {
  series_sum <- calculate_series_sum(n_terms)
  
  cat("The sum of the series up to", n_terms, "terms is:", series_sum, "\n")
}
