#' Printing Model Information
#' 
#' print method for objects of class 'modellist'.
#' 
#' @param x an object of class 'modellist'.
#' @param digits the number of significant digits to use when printing.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A data frame with one row per model, as printed, invisibly: the
#' specifications of \code{\link{get_model_specifications}}, without the
#' columns in which every model agrees.
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
#' models
#'
#' @export
print.modellist <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  
  result <- do.call("rbind", lapply(x, get_model_specifications))
  names(result)[which(names(result) == "type")] <- "Type"
  names(result)[which(names(result) == "varsel")] <- "Variable selection"
  
  # Drop columns, where all entries of a column are the same
  pos <- NULL
  for (i in names(result)) {
    if (!i %in% c("Type", "Variable selection", "k", "p")) {
      if (all(result[, i] == result[1, i])) {
        pos <- append(i, pos)
      } 
    }
  }
  if (!is.null(pos)) {
    result <- result[, -which(names(result) %in% pos)]
  }
  
  print(result, digits = digits, ...)
  invisible(result)
}
