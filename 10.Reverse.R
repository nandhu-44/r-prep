# Function to reverse a list using recursion
reverse_list <- function(lst) {
  # Base case: if the list is empty or has only one element, return the list itself
  if (length(lst) <= 1) {
    return(lst)
  }
  
  # Recursive case: reverse the rest of the list and append the first element to the reversed list
  return(c(reverse_list(lst[-1]), lst[1]))
}

# Main program
cat("Reverse List Program\n")

# Example list
input_list <- list(1, 2, 3, 4, 5)

# Reverse the list
reversed_list <- reverse_list(input_list)

# Print the reversed list
print(reversed_list)
