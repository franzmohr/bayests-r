#' Add Initial Values of an MCMC Chain
#'
#' Adds initial values to a list of models by passing each element to
#' the respective method.
#'
#' @param object a list of class 'expandingwindow', usually, the output of a call to
#' \code{\link{use_expanding_window}}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The object in \code{object} with initial values added to each of its models, as
#' described in \code{add_initial_values.bvarmodel} (\pkg{bvartools}).
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(y, iterations = 200)
#'
#' # One window per forecast origin from 2007Q4 on
#' windows <- use_expanding_window(model, start = 2008)
#' windows <- add_priors(windows, mean = 0, prior_variance = 10)
#' windows <- add_initial_values(windows)
#'
#' @export
add_initial_values.expandingwindow <- function(object, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_initial_values(object[[i]], ...)
  }
  
  return(object)
}
