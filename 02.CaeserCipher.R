# Function to apply Caesar Cipher
caesar_cipher <- function(sentence, shift) {
  # Function to shift a single character
  shift_char <- function(char, shift) {
    if (grepl("[a-zA-Z]", char)) {  # Check if the character is alphabetic
      base <- ifelse(char >= "a", "a", "A")
      shifted_char <- intToUtf8((utf8ToInt(char) - utf8ToInt(base) + shift) %% 26 + utf8ToInt(base))
      return(shifted_char)
    }
    return(char)  # Return non-alphabetic characters as is
  }
  
  # Split the sentence into characters and apply the shift
  chars <- unlist(strsplit(sentence, ""))
  encrypted <- sapply(chars, shift_char, shift = shift)
  return(paste0(encrypted, collapse = ""))
}

# Read input from the user
cat("Enter a sentence:\n")
sentence <- readLines(con = "stdin", n = 1)
cat("Enter the shift value (integer):\n")
shift <- as.integer(readLines(con = "stdin", n = 1))

# Perform encryption
encrypted_sentence <- caesar_cipher(sentence, shift)

# Output the result
cat("\nEncrypted Sentence:\n", encrypted_sentence, "\n")
