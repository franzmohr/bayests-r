#' Align Observations Across Models
#' 
#' Restricts each model in an object of class 'modellist' to the set of
#' observations common to all models, ensuring that comparisons are computed
#' on the same underlying sample.
#' 
#' @param object an object of class 'modellist'.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return An object of class 'modellist'.
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
#' @family model comparison
#' @export
align_model_obs.modellist <- function(object, ...) {
  
  # Get sample sizes
  avail <- lapply(object, function(x) {stats::tsp(x[["data"]][["train"]][["y"]])})
  avail <- do.call("rbind", avail)
  
  min_date <- max(avail[, 1]) # First period overall is latest in start
  max_date <- min(avail[, 2]) # First period overall is earlierst in end
  
  object <- stats::window(x = object, start = min_date, end = max_date, ...)
  
}
