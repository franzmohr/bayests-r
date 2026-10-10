#' Combine Models
#' 
#' Convenience function that combines multiple models into one object.
#' 
#' @param ... model objects, lists of class 'modellist' or 'expandingwindow',
#' or a mix of them. A model object is the list a model package returns, with
#' an element \code{model}.
#' 
#' 
#' @examplesIf requireNamespace("bvartools", quietly = TRUE) && utils::packageVersion("bvartools") >= "1.0.0"
#' library("bvartools")
#' 
#' # Load data
#' data(e6)
#' 
#' # Create multiple VAR models
#' model_1 <- create_bvarmodel(diff(e6), p = 0:2, deterministic = "const")
#' 
#' # Create multiple VEC models
#' model_2 <- create_bvecmodel(e6, p = 1, r = 0:1, const = "unrestricted")
#' 
#' # Combine the models in one object
#' model <- combine_models(model_1, model_2)
#' 
#' @return A list of class 'modellist' that holds the models of all arguments in one flat
#' list, in the order in which they were given.
#'
#' @family model set-up
#' @export
combine_models <- function(...) {
  
  input <- list(...)
  
  result <- NULL
  
  # A 'modellist' is spliced into the result; anything else that is a model,
  # or a list a model package treats as one -- an 'expandingwindow' -- is
  # added as it is. Models of any package are told apart by what they hold
  # rather than by a list of classes, which this package does not own.
  for (i in seq_along(input)) {
    x <- input[[i]]
    if (inherits(x, "modellist")) {
      result <- c(result, x)
    } else if (inherits(x, "expandingwindow") || .is_model_object(x)) {
      result <- c(result, list(x))
    } else {
      stop("Argument ", i, " of class '", paste(class(x), collapse = "', '"),
           "' is neither a model, a 'modellist' nor an 'expandingwindow'.")
    }
  }
  class(result) <- append("modellist", class(result))
  
  return(result)
  
}
