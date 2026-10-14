#' Thinning Posterior Draws
#' 
#' Thins the MCMC posterior draws of the elements in an object of class 'modellist'.
#' 
#' @param x an object of class 'modellist'.
#' @param thin an integer specifying the thinning interval between successive values of posterior draws.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return An object of class 'modellist'.
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
#' # Every fifth draw
#' models <- thin(models, thin = 5)
#'
#' @export
#' @method thin modellist
thin.modellist <- function(x, thin = 10, ...) {
  
  for (i in 1:length(x)) {
    x[[i]] <- thin(x[[i]], thin = thin, ...)
  }
 
  return(x)
}
