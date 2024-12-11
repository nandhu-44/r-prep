is_palindrome <- function(input_string) {
  clean_string <- tolower(gsub("[^a-zA-Z0-9]", "", input_string))
  
  return(clean_string == rev(strsplit(clean_string, NULL)[[1]]) %>% paste(collapse = ""))
}

cat("Enter a string to check if it's a palindrome: ")
input_string <- readline()

if (is_palindrome(input_string)) {
  cat("The string is a palindrome.\n")
} else {
  cat("The string is not a palindrome.\n")
}
