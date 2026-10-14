#' Add Priors to Bayesian Models
#'
#' A generic function used to generate prior vectors and matrices. The
#' function invokes particular methods which depend on the class of the first argument.
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
#' model <- create_normalmodel(y, iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' model[["priors"]]
#'
#' @seealso Methods: \code{\link[bvartools]{add_priors.bvarmodel}},
#' \code{\link[bvartools]{add_priors.bvecmodel}}, \code{\link{add_priors.expandingwindow}},
#' \code{add_priors.externalforecast} (\pkg{bvartools}), \code{\link{add_priors.modellist}}.
#'
#' @export
add_priors <- function (object, ...) {
 UseMethod("add_priors")
}
