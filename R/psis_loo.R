# Pareto smoothed importance sampling leave-one-out cross validation.
#
# Leave-one-out cross validation asks what the model would have said about an
# observation it had not seen. Refitting the sampler once per period is out of
# the question, so the posterior that did see period t is reweighted into the
# one that did not, which needs the importance ratio 1 / p(y_t | theta) for
# every draw. Those ratios have no variance to speak of for most periods and a
# very heavy right tail for the ones the model finds surprising, which is
# exactly where the plain estimate falls apart: a handful of draws carry all
# the weight.
#
# Pareto smoothing fits a generalised Pareto distribution to the largest
# ratios and replaces them by the quantiles of that fit, which keeps the tail
# but caps its influence. The shape parameter of the fit doubles as a
# diagnostic: above about 0.7 the reweighting cannot be trusted for that
# period, and the periods this happens for are the influential ones, so the
# count of them is reported rather than hidden.
#
# Following Vehtari, Simpson, Gelman, Yao and Gabry (2024), with the tail fit
# of Zhang and Stephens (2009) from posterior::gpdfit(), and on the deviance
# scale of the other criteria, so that smaller is better:
#
#   elpd_loo = sum_t log( sum_s w_st p(y_t | theta_s) / sum_s w_st )
#   p_loo    = lppd - elpd_loo
#   LOOIC    = -2 elpd_loo

.log_sum_exp <- function(x) {
  x_max <- max(x)
  if (!is.finite(x_max)) {
    return(x_max)
  }
  x_max + log(sum(exp(x - x_max)))
}

# Smoothed and normalised log weights of one period, with the shape parameter
# of the tail fit.
.psis_weights <- function(log_ratios, tail_len) {

  n_draws <- length(log_ratios)
  lw <- log_ratios - max(log_ratios)
  k_hat <- Inf

  # A tail of fewer than five values carries no information about its shape,
  # so it is left alone and flagged by an infinite k
  if (tail_len >= 5) {

    ord <- sort.int(lw, index.return = TRUE)
    tail_ids <- seq(n_draws - tail_len + 1, n_draws)
    lw_tail <- ord[["x"]][tail_ids]

    # A tail whose values are all equal has nothing to fit either
    if (abs(max(lw_tail) - min(lw_tail)) > .Machine$double.eps / 100) {

      cutoff <- ord[["x"]][min(tail_ids) - 1]
      exp_cutoff <- exp(cutoff)

      # The tail fit of Zhang and Stephens (2009), with the weakly informative
      # prior on the shape that pulls it towards 0.5, as the posterior
      # package implements it. It returns NA where it cannot fit -- when a
      # quarter of the tail or more sits at its smallest value -- which is
      # flagged by an infinite k like the other tails left unsmoothed.
      fit <- posterior::gpdfit(exp(lw_tail) - exp_cutoff, sort_x = FALSE)
      k_hat <- fit[["k"]]
      if (is.na(k_hat)) {
        k_hat <- Inf
      }

      if (is.finite(k_hat)) {
        probs <- (seq_len(tail_len) - 0.5) / tail_len
        smoothed <- posterior::qgeneralized_pareto(probs, mu = 0, sigma = fit[["sigma"]],
                                                   k = k_hat) + exp_cutoff
        lw[ord[["ix"]][tail_ids]] <- log(smoothed)
      }
    }
  }

  # Truncated at the largest raw ratio, which is zero after the shift above,
  # and normalised
  lw <- pmin(lw, 0)
  lw <- lw - .log_sum_exp(lw)

  list("log_weights" = lw, "pareto_k" = k_hat)
}

# Expects the draws x periods matrix of pointwise log-likelihoods that
# add_posterior_loglik() stores. Returns NULL when there are too few draws to
# reweight with, in the same way as .waic().
.psis_loo <- function(loglik) {

  loglik <- as.matrix(loglik)
  draws <- nrow(loglik)
  tt <- ncol(loglik)

  if (draws < 2) {
    return(NULL)
  }

  tail_len <- ceiling(min(0.2 * draws, 3 * sqrt(draws)))

  elpd <- numeric(tt)
  pareto_k <- numeric(tt)

  for (i in seq_len(tt)) {
    ll_i <- loglik[, i]
    # The importance ratio of dropping an observation is the reciprocal of its
    # likelihood, hence the sign
    smoothed <- .psis_weights(-ll_i, tail_len)
    elpd[i] <- .log_sum_exp(smoothed[["log_weights"]] + ll_i)
    pareto_k[i] <- smoothed[["pareto_k"]]
  }

  lppd <- apply(loglik, 2, function(z) {.log_sum_exp(z) - log(draws)})

  looic <- -2 * sum(elpd)
  # The standard error of a sum of T independent terms, on the deviance scale
  se <- 2 * sqrt(tt * stats::var(elpd))

  # Above this the reweighting of that period is not to be relied on. The
  # threshold tightens with the number of draws, as in Vehtari et al. (2024)
  threshold <- min(1 - 1 / log10(draws), 0.7)

  list("looic" = looic,
       "se" = se,
       "p_loo" = sum(lppd) - sum(elpd),
       "pareto_k" = pareto_k,
       "n_high_k" = sum(pareto_k > threshold),
       "k_threshold" = threshold)
}

# As for WAIC, the criterion is a point estimate rather than a quantity with a
# posterior distribution, so the quantile columns hold a normal interval built
# from its standard error. The diagnostics travel as attributes, which is what
# print.selcrit() reports the influential periods from.
.loo_data_frame <- function(loglik, ci_low, ci_high) {

  result <- .psis_loo(loglik)
  if (is.null(result)) {
    return(NULL)
  }

  z <- stats::qnorm(ci_high)

  out <- data.frame("mean" = result[["looic"]],
                    "median" = result[["looic"]],
                    "qlower" = result[["looic"]] - z * result[["se"]],
                    "qupper" = result[["looic"]] + z * result[["se"]])

  attr(out, "p_loo") <- result[["p_loo"]]
  attr(out, "n_high_k") <- result[["n_high_k"]]
  attr(out, "k_threshold") <- result[["k_threshold"]]

  out
}

# A note under the table of criteria, when the reweighting of some periods
# cannot be relied on. Those periods are the influential ones, and a LOOIC
# that rests on them says more about them than about the model, so the count
# belongs next to the number rather than in a corner of the documentation.
.print_loo_diagnostics <- function(looic) {

  if (is.null(looic)) {
    return(invisible(NULL))
  }

  n_high_k <- attr(looic, "n_high_k")
  if (is.null(n_high_k) || n_high_k == 0) {
    return(invisible(NULL))
  }

  threshold <- attr(looic, "k_threshold")

  cat("\nLOOIC: ", n_high_k, ifelse(n_high_k == 1, " period has", " periods have"),
      " a Pareto k above ", format(threshold, digits = 2),
      ", so its importance sampling is unreliable there.\n", sep = "")

  invisible(NULL)
}

# The same note for a list of models, which names the models whose LOOIC rests
# on periods the reweighting cannot handle rather than repeating a line per
# model.
.print_loo_diagnostics_list <- function(x) {

  n_high_k <- vapply(x, function(y) {
    value <- attr(y[["LOOIC"]], "n_high_k")
    if (is.null(value)) 0L else as.integer(value)
  }, integer(1))

  if (all(n_high_k == 0)) {
    return(invisible(NULL))
  }

  affected <- which(n_high_k > 0)
  threshold <- attr(x[[affected[1]]][["LOOIC"]], "k_threshold")

  cat("\nInfluential periods, whose importance sampling is unreliable, in model",
      ifelse(length(affected) == 1, " ", "s "),
      paste0(affected, " (", n_high_k[affected], ")", collapse = ", "),
      ", counted as a Pareto k above ", format(threshold, digits = 2), ".\n", sep = "")

  invisible(NULL)
}
