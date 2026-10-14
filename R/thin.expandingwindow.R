#' Thinning Posterior Draws
#' 
#' Thins the MCMC posterior draws of the elements in an object of class 'expandingwindow'.
#' 
#' @param x an object of class 'expandingwindow'.
#' @param thin an integer specifying the thinning interval between successive values of posterior draws.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return An object of class 'expandingwindow'.
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
#' windows <- thin(windows, thin = 5)
#'
#' @export
#' @method thin expandingwindow
thin.expandingwindow <- function(x, thin = 10, ...) {
  
  for (i in 1:length(x)) {
    x[[i]] <- thin(x[[i]], thin = thin, ...)
  }
 
  return(x)
}
