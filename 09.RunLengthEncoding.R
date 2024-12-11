# Function to compress a string using Run-Length Encoding
run_length_encoding <- function(input_string) {
  # Initialize variables
  encoded_string <- ""  # To store the compressed string
  count <- 1  # To count consecutive occurrences of a character
  
  # Loop through the string, starting from the second character
  for (i in 2:nchar(input_string)) {
    # If the current character is the same as the previous one
    if (substr(input_string, i, i) == substr(input_string, i - 1, i - 1)) {
      count <- count + 1  # Increment the count
    } else {
      # Append the current character and its count to the encoded string
      encoded_string <- paste0(encoded_string, substr(input_string, i - 1, i - 1), count)
      count <- 1  # Reset count for the next character
    }
  }
  
  # Append the last character and its count
  encoded_string <- paste0(encoded_string, substr(input_string, nchar(input_string), nchar(input_string)), count)
  
  return(encoded_string)
}

# Main program
cat("Run-Length Encoding Program\n")

# Read a string from the user
input_string <- readline(prompt = "Enter a string to compress: ")

# Compress the string using run-length encoding
encoded_string <- run_length_encoding(input_string)

# Display the compressed string
cat("Compressed String: ", encoded_string, "\n")
