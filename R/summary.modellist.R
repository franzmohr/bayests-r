#' Summarising Bayesian Models
#'
#' summary method for class 'modellist'.
#'
#' @param object an object of class 'modellist'.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A list of class 'summary.modellist'.
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
#'
#' summary(models)
#'
#' @export
#' @method summary modellist
summary.modellist <- function(object, ...){
  
  object <- lapply(object, summary, ...)
  
  class(object) <- c("summary.modellist", "list")
  
  return(object)
}
