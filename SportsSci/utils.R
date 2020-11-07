
mat_op_print <- function(..., width = 0) {
  # From @Stibu at StackOverflow
  # https://stackoverflow.com/questions/39419622/printing-matrices-and-vectors-side-by-side

  # get arguments
  args <- list(...)
  chars <- sapply(args, is.character)

  # auxilliary function to create character of n spaces
  spaces <- function(n) paste(rep(" ", n), collapse = "")

  # convert vectors to row matrix
  vecs <- sapply(args, is.vector)
  args[vecs & !chars] <- lapply(args[vecs & !chars], function(v) matrix(v, ncol = 1))

  # convert all non-characters to character with format
  args[!chars] <- lapply(args[!chars], format, width = width)

  # print names as the first line, if present
  arg_names <- names(args)
  if (!is.null(arg_names)) {
    get_title <- function(x, name) {
      if (is.matrix(x)) {
        paste0(name, spaces(sum(nchar(x[1, ])) + ncol(x) - 1 - nchar(name)))
      } else {
        spaces(nchar(x))
      }
    }
  cat(mapply(get_title, args, arg_names), "\n")
  }

  # auxiliary function to create the lines
  get_line <- function(x, n) {
    if (is.matrix(x)) {
      if (nrow(x) < n) {
       spaces(sum(nchar(x[1, ])) + ncol(x) - 1)
      } else {
        paste(x[n, ], collapse = " ")
      }
    } else if (n == 1) {
      x
    } else {
      spaces(nchar(x))
    }
  }

  # print as many lines as needed for the matrix with most rows
  N <- max(sapply(args[!chars], nrow))
  for (n in 1:N) {
    cat(sapply(args, get_line, n), "\n")
  }
}

