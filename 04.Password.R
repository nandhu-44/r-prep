# Function to generate a random password
generate_password <- function(length) {
    # Define character sets
    uppercase <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    lowercase <- "abcdefghijklmnopqrstuvwxyz"
    digits <- "0123456789"
    special <- "!@#$%^&*()_+-=[]{}|;:,.<>/?"

    # Combine all character sets
    all_chars <- paste0(uppercase, lowercase, digits, special)

    # Ensure the password contains at least one character from each set
    password <- c(
        sample(strsplit(uppercase, NULL)[[1]], 1),
        sample(strsplit(lowercase, NULL)[[1]], 1),
        sample(strsplit(digits, NULL)[[1]], 1),
        sample(strsplit(special, NULL)[[1]], 1)
    )

    # Fill the rest of the password length with random characters from all sets
    if (length > 4) {
        password <- c(password, sample(strsplit(all_chars, NULL)[[1]], length - 4, replace = TRUE))
    }

    # Shuffle the password characters and combine them into a single string
    password <- sample(password)
    password <- paste(password, collapse = "")

    return(password)
}

main <- function() {
    # Read the desired password length from the user
    length <- as.integer(readline(prompt = "Enter the desired password length: "))

    # Generate the password
    if (!is.na(length) && length >= 4) {
        password <- generate_password(length)
        cat("Generated Password:", password, "\n")
    } else {
        cat("Invalid length. Please enter a number greater than or equal to 4.\n")
    }
}

main()
