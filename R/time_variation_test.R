#' Test for Time Variation
#'
#' Generic function used to compute Bayes factors for time variation in the
#' coefficients and volatilities of a model with time varying parameters.
#'
#' @param object an object with suitable posterior draws passed forward to method.
#' @param ... arguments passed forward to method.
#'
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
#'
#' @seealso Methods: \code{\link[bvartools]{time_variation_test.bvarmodel}},
#' \code{\link[bvartools:time_variation_test.bvarmodel]{time_variation_test.bvecmodel}}, and for a list of models, the
#' windows of an expanding window comparison or a folder of stored models,
#' \code{\link{time_variation_test.modellist}},
#' \code{\link{time_variation_test.expandingwindow}} and
#' \code{\link[=folder_steps]{time_variation_test.modelfolder}}.
#'
#' @export
time_variation_test <- function(object, ...) {
  UseMethod("time_variation_test")
}


#' @export
time_variation_test.default <- function(object, ...) {
  stop("time_variation_test() has no method for an object of class '",
       paste(class(object), collapse = "', '"), "'. It is available for the models ",
       "of packages that define a method, estimated with a non-centred prior on ",
       "the variance of their states ('omega_v' in add_priors()), and for the ",
       "'modellist', 'expandingwindow' and 'modelfolder' objects that hold them.",
       call. = FALSE)
}

#' @export
print.bvartimevar <- function(x, digits = 2, ...) {

  cat("Bayes factors for time variation (Savage-Dickey, Chan 2018)\n")
  cat("log BF > 0 favours time variation; |log BF| > 3 is strong evidence\n\n")

  for (b in unique(x[["block"]])) {
    rows <- x[x[["block"]] == b, , drop = FALSE]
    out <- data.frame(equation = ifelse(is.na(rows[["equation"]]), "", rows[["equation"]]),
                      term = rows[["term"]],
                      `log BF` = formatC(rows[["log_bf"]], format = "f", digits = digits),
                      NSE = formatC(rows[["nse"]], format = "f", digits = digits),
                      check.names = FALSE, stringsAsFactors = FALSE)
    cat(toupper(substring(b, 1, 1)), substring(b, 2), ":\n", sep = "")
    print(out, row.names = FALSE, right = FALSE)
    cat("\n")
  }

  invisible(x)
}
