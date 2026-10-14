#' Add Priors to Bayesian Models
#'
#' Adds prior specifications to a list of models by passing each element to
#' the respective method.
#'
#' @param object a list of class 'modellist', usually, the output of a call
#' to \code{create_bvarmodel} (\pkg{bvartools}) or \code{create_bvecmodel} (\pkg{bvartools}).
#' @param ... further arguments passed to or from other methods.
#' 
#' @return The object in \code{object} with priors added to each of its models, as
#' described in \code{\link[bvartools]{add_priors.bvarmodel}}.
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
#'
#' @export
add_priors.modellist <- function(object, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_priors(object[[i]], ...)
  }
  
  return(object)
}
