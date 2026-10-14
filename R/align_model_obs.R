#' Align Observations Across Models
#'
#' Generic function used to restrict each model in a list to the set of
#' observations common to all models, ensuring that comparisons are computed
#' on the same underlying sample.
#' 
#' @param object a list of models.
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
#'
#' # Two models on samples that start in different periods
#' models <- combine_models(create_normalmodel(window(y, start = c(2001, 1))),
#'                          create_normalmodel(window(y, start = c(2003, 1))))
#' models <- align_model_obs(models)
#' sapply(models, function(x) start(x[["data"]][["train"]][["y"]]))
#'
#' @seealso Methods: \code{\link{align_model_obs.modellist}}.
#'
#' @export
align_model_obs <- function (object, ...) {
 UseMethod("align_model_obs")
}
