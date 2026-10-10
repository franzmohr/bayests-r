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
#' @seealso Methods: \code{add_forecast_errors.bvarmodel} (\pkg{bvartools}),
#' \code{\link{add_forecast_errors.expandingwindow}},
#' \code{\link{add_forecast_errors.modellist}}.
#'
#' @export
add_forecast_errors <- function (object, test_sample = NULL, ...) {
 UseMethod("add_forecast_errors")
}
