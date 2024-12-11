is_prime <- function(num) {
  if (num <= 1) {
    return(FALSE)
  }
  
  for (i in 2:sqrt(num)) {
    if (num %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

find_primes_in_range <- function(start, end) {
  primes <- c()
  
  for (num in start:end) {
    if (is_prime(num)) {
      primes <- c(primes, num)
    }
  }
  
  return(primes)
}

cat("Enter a number to check if it is prime: ")
number <- as.integer(readline())

if (is.na(number)) {
  cat("Invalid input. Please enter a valid integer.\n")
} else {
  if (is_prime(number)) {
    cat(number, "is a prime number.\n")
  } else {
    cat(number, "is not a prime number.\n")
  }
}

cat("Enter the start of the range: ")
start <- as.integer(readline())
cat("Enter the end of the range: ")
end <- as.integer(readline())

if (is.na(start) || is.na(end) || start > end) {
  cat("Invalid range input. Please enter valid integers with start <= end.\n")
} else {
  
  primes <- find_primes_in_range(start, end)
  

  if (length(primes) == 0) {
    cat("There are no prime numbers in the range", start, "to", end, ".\n")
  } else {
    cat("Prime numbers in the range", start, "to", end, "are:", paste(primes, collapse = ", "), "\n")
  }
}
