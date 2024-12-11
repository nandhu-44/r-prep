# Data Validation and User Input
# Develop a program to read student records with their names, ages, and grades.
# Implement validation checks:
#   a. Ensure age is a positive integer.
#   b. Ensure grade is a valid letter grade (A, B, C, D, F).
#   c. Calculate and display the average age of students with valid records.

# Function to validate age
validate_age <- function(age) {
  return(!(is.na(age) || age < 0 || age > 130))
}

# Function to validate grade
validate_grade <- function(grade) {
  if (grepl("^[A-F]$", grade) && grade != "E") {
    return(TRUE)
  }
  return(FALSE)
}

main <- function() {
students <- data.frame(name = character(), age = numeric(), grade = character(), stringsAsFactors = FALSE)

  # Read student records
  while (TRUE) {
    name <- readline(prompt = "Enter student name (or 'exit' to stop): ")
    if (tolower(name) == "exit") {
      break
    }

    age <- as.numeric(readline(prompt = "Enter student age: "))
    if (!validate_age(age)) {
      cat("Invalid age. Please enter a positive integer between 0 and 130.\n")
      next
    }

    grade <- readline(prompt = "Enter student grade (A, B, C, D, F): ")
    if (!validate_grade(grade)) {
      cat("Invalid grade. Please enter a valid letter grade (A, B, C, D, F).\n")
      next
    }

    students <- rbind(students, data.frame(name = name, age = age, grade = grade, stringsAsFactors = FALSE))
  }
  
  # Calculate and display average age of students
  if (nrow(students) > 0) {
    avg_age <- mean(students$age, na.rm = TRUE)
    cat("Average age of students with valid records: ", avg_age, "\n")
  } else {
    cat("No valid records found.\n")
  }
}

main()
