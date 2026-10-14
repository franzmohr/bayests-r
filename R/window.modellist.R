#' Time Series Windows
#' 
#' Forwards model input to the same function for individual models.
#' 
#' @param x an object of class 'modellist'.
#' @param start the start time of the period of interest.
#' @param end the end time of the period of interest.
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
#'
#' # The same shorter sample for all models
#' models <- window(models, start = c(2002, 1))
#'
#' @export
#' @method window modellist
window.modellist <- function(x, start = NULL, end = NULL, ...) {
  
  # '...' reaches every model, as the method's documentation says it does; it
  # used to stop here.
  for (i in seq_along(x)) {
    x[[i]] <- stats::window(x = x[[i]], start = start, end = end, ...)
  }
  
  return(x)
}
