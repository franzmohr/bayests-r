#' Add Forecast Errors
#'
#' Generic function used to calculate forecast errors and add them to a model object.
#' 
#' @param object an object of a class, for which a method should be called.
#' @param test_sample a time-series object used as test data. If \code{NULL}
#' (default), the values in \code{data$test$y} of the object are used, which is
#' what a model carries after it has been scored once and what a model read from
#' a file was written with.
#' @param ... arguments passed forward to method.
#' 
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(window(y, end = c(2008, 4)), iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' model <- add_initial_values(model)
#' model <- add_posterior_coefficients(model)
#' model <- add_forecast_input(model, n_ahead = 4)
#' model <- add_posterior_forecasts(model)
#' model <- add_forecast_errors(model, test_sample = y)
#'
#' @seealso Methods: \code{add_forecast_errors.bvarmodel} (\pkg{bvartools}),
#' \code{\link{add_forecast_errors.expandingwindow}},
#' \code{\link{add_forecast_errors.modellist}}.
#'
#' @export
add_forecast_errors <- function (object, test_sample = NULL, ...) {
 UseMethod("add_forecast_errors")
}
