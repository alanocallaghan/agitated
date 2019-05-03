assert_is <- function(x, class) {
  if (!inherits(x, class)) {
    stop(paste("x is not of class", class))
  }
}

