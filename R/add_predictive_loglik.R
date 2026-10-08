#' Add Predictive Log-Likelihood
#'
#' Generic function that adds the draws of the one-step-ahead log predictive
#' density to the windows of an expanding window exercise, which
#' \code{\link{selection_criteria}} sums to the log predictive likelihood of
#' Geweke and Amisano (2011).
#'
#' @param object an object of class 'expandingwindow' whose windows hold
#' posterior draws, or a 'modellist' of such objects, as returned by
#' \code{\link{use_expanding_window}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#' The windows of an expanding window exercise grow by one period at a time, so
#' window \eqn{i + 1} holds exactly one observation that window \eqn{i} has
#' not seen. A method evaluates the density of that observation at each
#' posterior draw of window \eqn{i}, given only the data before it, and stores
#' the draws in element \code{predictive} of the window, together with the
#' period of the observation. How a draw is carried one period forward depends
#' on the model, which is why the method for an expanding window comes with the
#' package of its models: \code{\link[bvartools]{add_predictive_loglik.expandingwindow}}
#' for the VAR and VEC models of \pkg{bvartools}. The method for a 'modellist'
#' applies it to every window exercise of the list.
#'
#' @return The object in \code{object}, with element \code{predictive} added to
#' every window but the last. It is a list with \code{loglik}, the draws of the
#' log predictive density, and \code{period}, the time of the predicted
#' observation.
#'
#' @references
#'
#' Geweke, J., & Amisano, G. (2011). Hierarchical Markov normal mixture models with
#' applications to financial asset returns. \emph{Journal of Applied Econometrics, 26}(1),
#' 1--29. \doi{10.1002/jae.1119}
#'
#' @family model comparison
#' @export
add_predictive_loglik <- function(object, ...) {
  UseMethod("add_predictive_loglik")
}

#' @rdname add_predictive_loglik
#' @export
add_predictive_loglik.modellist <- function(object, ...) {

  orig_class <- class(object)
  object <- lapply(object, add_predictive_loglik, ...)
  class(object) <- orig_class

  return(object)
}

# Log of the mean of exp(x), and its numerical standard error by the delta
# method, with batch means for the autocorrelation of the chain.
.log_mean_exp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

# The 'LPL' entry of a set of criteria, built from a sequence of one step ahead
# predictive densities.
#
# 'densities' is a list of draws of the log predictive density, one element per
# period, and 'periods' the times of the observations they score. The log of the
# mean of exp() of each is that period's log predictive density, and their sum
# is the log predictive likelihood.
#
# Both sources reach this. An expanding window exercise has one density per
# window, taken by add_predictive_loglik() from the draws of that window; a
# single model has one per horizon of its forecast, taken by BayesTS against the
# values in data$test$y and read back from posterior$forecast$loglik. They are
# the same quantity computed two ways, and going through one function is what
# makes them agree by construction rather than by coincidence.
#
# The band treats the terms as independent, which they are not -- consecutive
# windows share all but one observation, and consecutive horizons share a
# forecast origin -- so it is a rough guide to how much of the total is sampling
# noise rather than an interval to test with. The numerical standard error
# beside it is a different thing: what the length of the chain contributes, which
# more draws would shrink and more data would not.
.lpl_entry <- function(densities, periods, ci_low, ci_high) {

  terms <- data.frame(
    period = as.numeric(periods),
    lpd = vapply(densities, .log_mean_exp, numeric(1)),
    nse = vapply(densities, .nse_log_mean_exp, numeric(1)))

  n_terms <- nrow(terms)
  lpl <- sum(terms[["lpd"]])
  se <- if (n_terms > 1) sqrt(n_terms * stats::var(terms[["lpd"]])) else NA_real_
  z <- stats::qnorm(ci_high)

  entry <- data.frame(mean = lpl, median = NA_real_,
                      qlower = lpl - z * se, qupper = lpl + z * se)
  attr(entry, "terms") <- terms
  attr(entry, "nse") <- sqrt(sum(terms[["nse"]]^2))

  return(entry)
}


.nse_log_mean_exp <- function(x, batches = 20) {
  batches <- min(batches, floor(length(x) / 2))
  if (batches < 2) {
    return(NA_real_)
  }
  e <- exp(x - max(x))
  means <- vapply(split(e, cut(seq_along(e), batches, labels = FALSE)), mean, numeric(1))
  sqrt(stats::var(means) / batches) / mean(e)
}
