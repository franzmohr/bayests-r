#' Add Forecasts
#' 
#' Generic function that calculates and adds forecasts.
#' 
#' @param object an object with suitable input data passed forward to method.
#' @param ... arguments passed forward to method.
#' 
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
#'
#' @seealso Methods: \code{add_posterior_forecasts.bvarmodel} (\pkg{bvartools}),
#' \code{\link{add_posterior_forecasts.expandingwindow}},
#' \code{add_posterior_forecasts.externalforecast} (\pkg{bvartools}),
#' \code{\link{add_posterior_forecasts.modellist}}.
#'
#' @export
add_posterior_forecasts <- function (object, ...) {
  UseMethod("add_posterior_forecasts")
}
