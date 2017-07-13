#!/usr/bin/Rscript
system_in <- file("stdin")
open(system_in)

while (length(line <- readLines(system_in, n = 1)) > 0) {
  words <-unlist(strsplit(trimws(line), " "))
  for (word in words) { # this is probably really slow in R
    cat(paste0(word, "\t1", "\n"))
  }
}
