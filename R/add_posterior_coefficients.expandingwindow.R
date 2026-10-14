#' Posterior Simulation of Model Coefficients
#' 
#' Forwards model input to posterior simulation functions.
#' 
#' @param object an object of class 'expandingwindow'.
#' @param ... further arguments passed to or from other methods.
#' @inheritParams add_posterior_coefficients.modellist
#' @inheritSection add_posterior_coefficients.modellist Parallel simulation
#' 
#' @return The object in \code{object} with posterior draws added to each of its models, as
#' described in \code{add_posterior_coefficients.bvarmodel} (\pkg{bvartools}).
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(y, iterations = 200)
#'
#' # One window per forecast origin from 2007Q4 on
#' windows <- use_expanding_window(model, start = 2008)
#' windows <- add_priors(windows, mean = 0, prior_variance = 10)
#' windows <- add_initial_values(windows)
#' windows <- add_posterior_coefficients(windows)
#'
#' @export
add_posterior_coefficients.expandingwindow <- function(object, ..., cores = 1){

  if (.use_cluster(object, cores)) {
    return(.simulate_models_in_parallel(object, "add_posterior_coefficients", cores, ...,
                                        seed = TRUE))
  }

  object <- lapply(object, add_posterior_coefficients, ...)
  
  class(object) <- list("expandingwindow", "list")
  
  return(object)
}
