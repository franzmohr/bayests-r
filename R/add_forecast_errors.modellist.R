#' Add Forecast Errors
#'
#' Calculates and adds forecast errors for a list of Bayesian models.
#'
#' @param object an object of class 'modellist'.
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
#' train <- window(y, end = c(2008, 4))
#'
#' # Two models that differ in the variance they take as known
#' models <- combine_models(create_normalmodel(train, variance = 1, iterations = 200),
#'                          create_normalmodel(train, variance = 4, iterations = 200))
#' models <- add_priors(models, mean = 0, prior_variance = 10)
#' models <- add_initial_values(models)
#' models <- add_posterior_coefficients(models)
#' models <- add_forecast_input(models, n_ahead = 4)
#' models <- add_posterior_forecasts(models)
#' models <- add_forecast_errors(models, test_sample = y)
#'
#' @export
add_forecast_errors.modellist <- function(object, test_sample = NULL, ...){
  
  orig_class <- class(object)
  object <- lapply(object, add_forecast_errors, test_sample = test_sample, ...)
  class(object) <- orig_class
  
  return(object)
}
