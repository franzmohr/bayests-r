#' Add Forecast Input Data
#' 
#' Generic function used to generate and add data matrices for forecast simulation
#' to a model object.
#' 
#' @param object an object of a class, for which a method should be called.
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
#'
#' @seealso Methods: \code{add_forecast_input.bvarmodel} (\pkg{bvartools}),
#' \code{add_forecast_input.bvecmodel} (\pkg{bvartools}),
#' \code{\link{add_forecast_input.expandingwindow}},
#' \code{add_forecast_input.externalforecast} (\pkg{bvartools}),
#' \code{\link{add_forecast_input.modellist}}.
#'
#' @export
add_forecast_input <- function (object, ...) {
  UseMethod("add_forecast_input")
}
