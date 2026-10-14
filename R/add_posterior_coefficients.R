#' Posterior Simulation of Model Coefficients
#' 
#' Generic function used for posterior simulation of model coefficients.
#' 
#' @param object an object of a class, for which a method should be called.
#' @param ... arguments passed forward to method.
#'
#' @details Before the method is called, the size the object will have once its
#' draws are complete is compared with \code{options(bayests.size_warning)},
#' and a warning is given if it is larger. See \code{\link{expected_model_size}}.
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
#' summary(model)
#'
#' @seealso Methods: \code{add_posterior_coefficients.bvarmodel} (\pkg{bvartools}),
#' \code{add_posterior_coefficients.bvecmodel} (\pkg{bvartools}),
#' \code{\link{add_posterior_coefficients.expandingwindow}},
#' \code{add_posterior_coefficients.externalforecast} (\pkg{bvartools}),
#' \code{\link{add_posterior_coefficients.modellist}}.
#'
#' @export
add_posterior_coefficients <- function(object, ...){
  # Once per call from the outside, for the whole of a list of models rather
  # than for each member the collection methods pass back through here.
  if (!isTRUE(.size_check[["posterior"]]) && .check_posterior_size(object, list(...)[["chains"]])) {
    .size_check[["posterior"]] <- TRUE
    on.exit(.size_check[["posterior"]] <- FALSE, add = TRUE)
  }
  UseMethod("add_posterior_coefficients")
}
