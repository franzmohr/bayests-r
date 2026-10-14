#' Expanding Window Estimation
#' 
#' Creates objects for expanding window posterior simulation.
#' 
#' @param object an object containing model specifications and input data.
#' Usually, the output of a call to  \code{create_bvarmodel} (\pkg{bvartools}) or
#' \code{create_bvecmodel} (\pkg{bvartools}).
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
#'
#' # One window per forecast origin from 2007Q4 on
#' windows <- use_expanding_window(model, start = 2008)
#' windows <- add_priors(windows, mean = 0, prior_variance = 10)
#'
#' length(windows)
#' end(windows[[1]][["data"]][["train"]][["y"]])
#'
#' @seealso Methods: \code{use_expanding_window.bvarmodel} (\pkg{bvartools}),
#' \code{use_expanding_window.bvecmodel} (\pkg{bvartools}),
#' \code{\link{use_expanding_window.modellist}}.
#'
#' @export
use_expanding_window <- function(object, ...) {
  UseMethod("use_expanding_window")
}
