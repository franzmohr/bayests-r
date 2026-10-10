#' Impulse Response Function
#'
#' A generic function used to calculate impulse response functions.
#' 
#' @param x an object with suitable input data passed forward to method.
#' @param ... arguments passed forward to method.
#' 
#' @return The value returned by the method for the class of \code{x},
#' as described on the pages of the methods.
#'
#' @seealso Methods: \code{irf.bvarmodel} (\pkg{bvartools}).
#'
#' @export
irf <- function (x, ...) {
 UseMethod("irf")
}
