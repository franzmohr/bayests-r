#' Add Forecast Input Data
#'
#' Generates and adds data matrices for forecast simulation to the elements of
#' an object of class 'expandingwindow'.
#'
#' @param object an object of class 'expandingwindow' containing objects that can
#' be passed on to their respective \code{add_forecast_input} method.
#' @param ... arguments passed forward to method. Argument \code{y_start} of
#' \code{add_forecast_input.bvarmodel} (\pkg{bvartools}) is refused, since every window forecasts from
#' the end of its own estimation sample.
#'
#' @return The object in \code{object} with forecast input added to each of its models, as
#' described in \code{add_forecast_input.bvarmodel} (\pkg{bvartools}).
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
#'
#' @export
#' @method add_forecast_input expandingwindow
add_forecast_input.expandingwindow <- function(object, ...){

  # Every window forecasts the periods after its own sample, which is what the
  # exercise compares. One set of start values would make them all start in the
  # same period.
  if (!is.null(list(...)[["y_start"]])) {
    stop("Argument 'y_start' is not available for an 'expandingwindow' object: each window ",
         "forecasts from the end of its own estimation sample. Give it to add_forecast_input() ",
         "of a single model instead, such as the last window, object[[length(object)]].",
         call. = FALSE)
  }

  for (i in 1:length(object)) {
    object[[i]] <- add_forecast_input(object[[i]], ...)
  }
  
  return(object)
}
