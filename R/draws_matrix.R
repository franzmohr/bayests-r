# The draws of an mcmc object as a plain matrix, one row per draw, and with the
# dimnames it had. as.matrix() would do the first but add an empty dimnames list
# to draws that had none, so that thinning by one or cutting a window to the
# whole sample no longer returned what it was given.
.draws_matrix <- function(draws) {
  attr(draws, "mcpar") <- NULL
  class(draws) <- NULL
  if (is.null(dim(draws))) {
    draws <- matrix(draws, ncol = 1)
  }
  return(draws)
}
