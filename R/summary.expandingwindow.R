#' Summarising Bayesian Models
#'
#' summary method for class 'expandingwindow'.
#'
#' @param object an object of class 'expandingwindow'.
#' @param ... further arguments passed to or from other methods.
#' 
#' @details
#' The method passes the last element of the expanding window model list to its
#' corresponding summary function; assuming that this object contains the model
#' with the highest amount of available observations.
#' 
#'
#' @return A list of class 'summary.bvarmodel' or 'summary.bvecmodel'.
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
#' windows <- add_posterior_coefficients(windows)
#'
#' summary(windows)[[1]]
#'
#' @export
summary.expandingwindow <- function(object, ...){
  
  # Only evaluate the last model with the highest amount of observations
  object <- summary(object[[length(object)]])
  
  return(object)
}
