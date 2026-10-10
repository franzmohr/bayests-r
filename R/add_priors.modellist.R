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
#' @examplesIf requireNamespace("bvartools", quietly = TRUE) && utils::packageVersion("bvartools") >= "1.0.0"
#' library("bvartools")
#' 
#' # Load data
#' data("e1")
#' e1 <- diff(log(e1)) * 100
#' 
#' # Create model
#' model <- create_bvarmodel(e1, p = 0:2, deterministic = "const",
#'                           iterations = 10, burnin = 10)
#' # Number of iterations and burnin should be much higher.
#' 
#' # Add prior specifications
#' model <- add_priors(model,
#'                     coef = list(v_i = 1, v_i_det = 1 / 10),
#'                     sigma = list(df = "k", scale = 1))
#' 
#' @export
add_priors.modellist <- function(object, ...){
  
  for (i in 1:length(object)) {
    object[[i]] <- add_priors(object[[i]], ...)
  }
  
  return(object)
}
