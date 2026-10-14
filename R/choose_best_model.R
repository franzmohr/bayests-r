#' Choose Best Model
#' 
#' A generic function that applies model selection criteria to find the best
#' model.
#' 
#' @param object an object with suitable input data passed forward to method.
#' @param ... arguments passed forward to method.
#' 
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
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
#' criteria <- selection_criteria(models)
#' # The model whose variance is close to that of the data
#' choose_best_model(criteria, criterion = "WAIC")
#'
#' @seealso Methods: \code{\link{choose_best_model.selcritlist}}.
#'
#' @export
choose_best_model <- function (object, ...) {
  UseMethod("choose_best_model")
}
