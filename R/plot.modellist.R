#' Plotting Draws of a Bayesian Time Series Models
#' 
#' A plot function for objects of class 'modellist'.
#' 
#' @param x an object of class 'modellist'.
#' @param ... arguments passed forward to other methods.
#'
#' @return \code{x}, invisibly. The function is called for its side effect, one
#' plot per model in the list, drawn by the plot method of that model.
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
#' plot(models)
#'
#' @export
plot.modellist <- function(x, ...) {

  for (i in seq_along(x)) {
    plot(x[[i]], ...)
  }

  invisible(x)
}


