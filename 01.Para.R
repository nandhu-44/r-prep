# Develop a program to read a paragraph of text and perform the following tasks:
# a) Count the total number of words
# b) Calculate the average word length
# c) Identify and print the longest word
# d) Replace all occurrences of a specific word with another word of your choice

content <- readLines("paragraph.txt")

# Count the total number of words
words <- unlist(strsplit(content, "\\s+"))
word_count <- length(words)
cat("\n\nWord Count:", word_count, "\n")

# Calculate the average word length
word_lengths <- nchar(words)
average_word_length <- sum(word_lengths) / word_count
cat("Average Word Length:", average_word_length, "\n")

# Identify and print the longest word
longest_word <- words[which.max(nchar(words))]
cat("Longest Word:", longest_word, "\n")

# Replace all occurrences of a specific word with another word of your choice
word_to_replace <- "ipsum"
replacement_word <- "🐼 IPSUM 🐼"

content <- gsub(word_to_replace, replacement_word, content)

# print the updated content
cat("\n\nUpdated Content:\n")
cat(content, sep = "\n")
