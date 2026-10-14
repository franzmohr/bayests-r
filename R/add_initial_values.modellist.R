#' Add Initial Values of an MCMC Chain
#'
#' Adds initial values to a list of models by passing each element to
#' the respective method.
#'
#' @param object a list of class 'modellist' \code{\link{add_priors}}.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The object in \code{object} with initial values added to each of its models, as
#' described in \code{add_initial_values.bvarmodel} (\pkg{bvartools}).
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
#'
#' @export
add_initial_values.modellist <- function(object, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_initial_values(object[[i]], ...)
  }
  
  return(object)
}
