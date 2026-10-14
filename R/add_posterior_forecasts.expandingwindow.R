#' Add Forecasts
#'
#' Calculates and adds forecasts to the elements of an object of class 'expandingwindow'.
#'
#' @param object an object of class 'expandingwindow', usually, the result of a call
#' to \code{\link{add_posterior_coefficients}} and \code{\link{add_forecast_input}}.
#' @param ... arguments passed forward to method.
#' @inheritParams add_posterior_coefficients.modellist
#' @inheritSection add_posterior_coefficients.modellist Parallel simulation
#' 
#' @return The object in \code{object} with forecast draws added to each of its models, as
#' described in \code{add_posterior_forecasts.bvarmodel} (\pkg{bvartools}).
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
#' windows <- add_forecast_input(windows, n_ahead = 2)
#' windows <- add_posterior_forecasts(windows)
#'
#' @export
add_posterior_forecasts.expandingwindow <- function(object, ..., cores = 1){

  if (.use_cluster(object, cores)) {
    return(.simulate_models_in_parallel(object, "add_posterior_forecasts", cores, ...))
  }
  
  orig_class <- class(object)
  object <- lapply(object, add_posterior_forecasts, ...)
  class(object) <- orig_class
  
  return(object)
}
