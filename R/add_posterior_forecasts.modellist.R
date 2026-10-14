#' Add Forecasts
#'
#' Calculates and adds forecasts to the elements of an object of class 'modellist'.
#'
#' @param object an object of class 'modellist', usually, the result of a call
#' to \code{\link{add_posterior_coefficients}} and \code{\link{add_forecast_input}}.
#' @param ... arguments passed forward to method.
#' @inheritParams add_posterior_coefficients.modellist
#' @inheritSection add_posterior_coefficients.modellist Parallel simulation
#' 
#' @return The object in \code{object} with forecast draws added to each of its models, as
#' described in \code{add_posterior_forecasts.bvarmodel} (\pkg{bvartools}).
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
#' models <- add_forecast_input(models, n_ahead = 4)
#' models <- add_posterior_forecasts(models)
#'
#' @export
add_posterior_forecasts.modellist <- function(object, ..., cores = 1){

  if (.use_cluster(object, cores)) {
    return(.simulate_models_in_parallel(object, "add_posterior_forecasts", cores, ...))
  }
  
  for (i in 1:length(object)) {
    object[[i]] <- add_posterior_forecasts(object[[i]], ...)
  }
  
  return(object)
}
