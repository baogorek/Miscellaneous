#!/usr/bin/Rscript
system_in <- file("stdin")
open(system_in)

word_in_register <- "" 
word_in_line <- "" 
count_in_register <- 0

while (length(line <- readLines(system_in, n = 1)) > 0) {

  line_contents <- unlist(strsplit(trimws(line), "\t"))
  word_in_line <- line_contents[1]
  value <- as.integer(line_contents[2])

  if (word_in_line == word_in_register) {
    count_in_register <- count_in_register + value
  } else {
    if (count_in_register > 0) {
      cat(paste0(word_in_register, "\t", count_in_register, "\n"))
    }
    word_in_register <- word_in_line
    count_in_register <- value
  } 
}
# Last word
if (word_in_register == word_in_line) {
  cat(paste0(word_in_register, "\t", count_in_register, "\n"))
}
