#' Add Forecast Input Data
#'
#' Generates and adds data matrices for forecast simulation to the elements of
#' an object of class 'modellist'.
#'
#' @param object an object of class 'modellist' containing objects that can
#' be passed on to their respective \code{add_forecast_input} method.
#' @param ... arguments passed forward to method. Argument \code{y_start} of
#' \code{add_forecast_input.bvarmodel} (\pkg{bvartools}) is given to every model of the list, so it
#' has to cover the largest lag order among them.
#'
#' @return The object in \code{object} with forecast input added to each of its models, as
#' described in \code{add_forecast_input.bvarmodel} (\pkg{bvartools}).
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
#'
#' @export
#' @method add_forecast_input modellist
add_forecast_input.modellist <- function(object, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_forecast_input(object[[i]], ...)
  }
  
  return(object)
}
