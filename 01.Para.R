para <- function() {
  cat("Enter your paragraph:\n")
  paragraph <- readline()
  words <- unlist(strsplit(paragraph, "\\s+"))
  # (a)
  words_number <- length(words)
  cat("\nThe total number of words =", words_number, "\n")
  # (b)
  word_len <- nchar(words)
  avg_word_len <- mean(word_len)
  cat("Average word length =", avg_word_len, "\n")
  # (c)
  longest_len <- max(word_len)
  longest_word <- words[word_len == longest_len]
  # longest_word <- longest_words[1]
  # In case there are multiple words with the same length
  cat("The longest word is:", longest_word, "\n")
  # (d)
  cat("Enter the word to be replaced: ")
  replace <- readline()
  cat("Enter the word to replace with: ")
  new <- readline()
  modified_para <- modified(replace, new, paragraph)
  cat("The modified paragraph is:\n", modified_para, "\n")
}


modified <- function(replace, new, paragraph) {
  words <- unlist(strsplit(paragraph, "\\s+"))
  mod <- sapply(words, function(word) {
    if (word == replace) {
      return(new)
    } else {
      return(word)
    }
  })
  modified_para <- paste(mod, collapse = " ")
  return(modified_para)
}
para()
