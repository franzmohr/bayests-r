#' Add Forecast Errors
#'
#' Calculates and adds forecast errors for a list of Bayesian models.
#'
#' @param object an object of class 'expandingwindow'.
#' @param test_sample a time-series object used as test data. If \code{NULL}
#' (default), the values in \code{data$test$y} of the object are used.
#' @param ... arguments passed forward to method.
#' 
#' @return The object in \code{object} with forecast errors added to each of its models, as
#' described in \code{add_forecast_errors.bvarmodel} (\pkg{bvartools}).
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
#' # The windows whose forecasts have been realised get their errors
#' windows <- add_forecast_errors(windows, test_sample = y)
#'
#' @export
add_forecast_errors.expandingwindow <- function(object, test_sample = NULL, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_forecast_errors(object[[i]], test_sample = test_sample, ...)
  }
  
  return(object)
}
