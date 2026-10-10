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
#' @seealso Methods: \code{use_expanding_window.bvarmodel} (\pkg{bvartools}),
#' \code{use_expanding_window.bvecmodel} (\pkg{bvartools}),
#' \code{\link{use_expanding_window.modellist}}.
#'
#' @export
use_expanding_window <- function(object, ...) {
  UseMethod("use_expanding_window")
}
