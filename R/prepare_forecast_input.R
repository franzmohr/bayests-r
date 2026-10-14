#' Prepare Forecast Input
#' 
#' Generic function that generates data matrices serving as data input for downstream
#' forecasting functions.
#' 
#' @param object an object with suitable input data passed forward to method.
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
#' model <- create_normalmodel(y, iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' prepare_forecast_input(model, n_ahead = 4)
#'
#' @seealso Methods: \code{prepare_forecast_input.bvarmodel} (\pkg{bvartools}),
#' \code{prepare_forecast_input.bvecmodel} (\pkg{bvartools}).
#'
#' @export
prepare_forecast_input <- function (object, ...) {
  UseMethod("prepare_forecast_input")
}
