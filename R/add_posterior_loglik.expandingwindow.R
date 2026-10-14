#' Add Log-Likelihood
#' 
#' Forwards its elements to methods used to calculate posterior log-likelihoods.
#' 
#' @param object an object of class 'expandingwindow', usually, the result of a call to
#' \code{\link{add_posterior_coefficients}}.
#' @param ... further arguments passed to or from other methods.
#' @inheritParams add_posterior_coefficients.modellist
#' @inheritSection add_posterior_coefficients.modellist Parallel simulation
#' 
#' @return The object in \code{object} with log-likelihood draws added to each of its models, as
#' described in \code{add_posterior_loglik.bvarmodel} (\pkg{bvartools}).
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
#' windows <- add_posterior_loglik(windows)
#'
#' @export
add_posterior_loglik.expandingwindow <- function(object, ..., cores = 1){

  if (.use_cluster(object, cores)) {
    return(.simulate_models_in_parallel(object, "add_posterior_loglik", cores, ...))
  }
  
  orig_class <- class(object)
  object <- lapply(object, add_posterior_loglik, ...)
  class(object) <- orig_class
  
  return(object)
}
