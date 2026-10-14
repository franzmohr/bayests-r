#' Expanding Window Estimation
#' 
#' Creates objects for expanding window posterior simulation.
#' 
#' @param object a list of objects containing model specifications and input data.
#' Usually, the output of a call to a model creation function such as
#' \code{create_bvarmodel} (\pkg{bvartools}) or \code{create_bvecmodel} (\pkg{bvartools}).
#' @param start the start period of the prediction of the first iteration of the
#' expanding window approach.
#' @param ... arguments passed forward to method.
#' 
#' @return A list of class 'modellist', which consists of objects of class
#' 'expandingwindow'.
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
#' # Each model becomes a list of windows
#' windows <- use_expanding_window(models, start = 2007)
#' length(windows[[1]])
#'
#' @export
#' @method use_expanding_window modellist
use_expanding_window.modellist <- function(object, start, ...) {
  
  result <- list()
  for (i in 1:length(object)) {
    result[[i]] <- use_expanding_window(object[[i]], start = start, ...)
  }
  
  class(result) <- append("modellist", class(result))
  
  return(result)
}
