#' Model Selection Criteria
#'
#' Calculates model selection criteria for a list of Bayesian models.
#'
#' @param object an object of class 'modellist'.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return A list of class 'selcritlist' with one object of class 'selcrit' per model, as
#' described in \code{selection_criteria.bvarmodel} (\pkg{bvartools}), which
#' \code{\link{choose_best_model}} compares.
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
#' selection_criteria(models)
#'
#' @family model comparison
#' @export
#' @method selection_criteria modellist
selection_criteria.modellist <- function(object, ...){
  
  object <- lapply(object, selection_criteria, ...)
  
  class(object) <- append("selcritlist", class(object))
  
  return(object)
}
