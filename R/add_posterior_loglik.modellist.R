#' Add Log-Likelihood
#' 
#' Forwards its elements to methods used to calculate posterior log-likelihoods.
#' 
#' @param object a list of class 'modellist', usually, the result of a call to
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
#' train <- window(y, end = c(2008, 4))
#'
#' # Two models that differ in the variance they take as known
#' models <- combine_models(create_normalmodel(train, variance = 1, iterations = 200),
#'                          create_normalmodel(train, variance = 4, iterations = 200))
#' models <- add_priors(models, mean = 0, prior_variance = 10)
#' models <- add_initial_values(models)
#' models <- add_posterior_coefficients(models)
#' models <- add_posterior_loglik(models)
#'
#' @export
add_posterior_loglik.modellist <- function(object, ..., cores = 1){

  if (.use_cluster(object, cores)) {
    return(.simulate_models_in_parallel(object, "add_posterior_loglik", cores, ...))
  }
  
  object <- lapply(object, add_posterior_loglik, ...)
  class(object) <- list("modellist", "list")
  
  return(object)
}
