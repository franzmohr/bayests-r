#' Add Log-Likelihood
#' 
#' Generic function that calculates the posterior log-likelihoods of a model.
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
#' model <- add_initial_values(model)
#' model <- add_posterior_coefficients(model)
#' model <- add_posterior_loglik(model)
#'
#' @seealso Methods: \code{add_posterior_loglik.bvarmodel} (\pkg{bvartools}),
#' \code{add_posterior_loglik.bvecmodel} (\pkg{bvartools}),
#' \code{\link{add_posterior_loglik.expandingwindow}},
#' \code{add_posterior_loglik.externalforecast} (\pkg{bvartools}),
#' \code{\link{add_posterior_loglik.modellist}}.
#'
#' @export
add_posterior_loglik <- function (object, ...) {
  UseMethod("add_posterior_loglik")
}
