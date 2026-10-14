#' A Normal Model of the Mean with Known Variance
#'
#' A conjugate model for the mean of each of a set of time series, which this
#' package ships so that the model workflow, the lists of models and the
#' comparison of models can be shown without a package that defines model
#' classes, such as 'bvartools'.
#'
#' @param data a time series object, a single series or a matrix of them.
#' @param variance the variance of each series, which the model treats as
#' known: a single value for all series or one per series. If \code{NULL}
#' (the default), the sample variance of each series is used.
#' @param iterations an integer of the number of draws of the posterior per
#' chain.
#' @param object,x an object of class \code{"normalmodel"}.
#' @param mean,prior_variance the mean and the variance of the normal prior on
#' the mean of every series.
#' @param seed a whole number, the seed of the model's draws.
#' @param chains the number of chains, each of \code{iterations} draws.
#' @param n_ahead the number of periods to forecast.
#' @param test_sample a time series object holding the realised values of the
#' periods that were forecast. If \code{NULL}, those in \code{data$test$y}.
#' @param start the period of the first window's forecast, or the start of the
#' sample to keep.
#' @param end the end of the sample to keep.
#' @param thin an integer, every how many draws to keep.
#' @param filename a path to the model file to write.
#' @param group the group of the file to write the model to, \code{""} for the
#' root of the file.
#' @param tree a list of a model file's contents, as
#' \code{\link{read_bayests_tree}} returns it.
#' @param ... further arguments, ignored.
#'
#' @details
#' Each series \eqn{y_{i}} is normal with mean \eqn{\mu_i} and a known variance
#' \eqn{\sigma^2_i}, independently over periods and series, and \eqn{\mu_i} has
#' a normal prior with mean \eqn{m_0} and variance \eqn{v_0}. The posterior of
#' \eqn{\mu_i} after \eqn{T} periods is then normal with variance
#' \eqn{v_1 = (1 / v_0 + T / \sigma^2_i)^{-1}} and mean
#' \eqn{v_1 (m_0 / v_0 + \sum_t y_{it} / \sigma^2_i)}, so its draws are
#' independent and need no burn-in.
#'
#' The model is small on purpose. It is not meant for analysis, but it has the
#' methods of a model package: \code{add_priors()}, \code{add_initial_values()},
#' \code{add_seed()}, \code{add_posterior_coefficients()},
#' \code{add_posterior_loglik()}, \code{add_forecast_input()},
#' \code{add_posterior_forecasts()}, \code{predict()}, \code{add_forecast_errors()},
#' \code{add_predictive_loglik()}, \code{use_expanding_window()},
#' \code{window()}, \code{thin()}, \code{expected_model_size()},
#' \code{write_to_hdf5()} and the reading of its model file. That is what lets
#' the examples of this package run on it.
#'
#' \strong{The variance is not estimated.} When \code{variance} is not given,
#' the sample variance of each series is plugged in and then treated as known,
#' so the posterior is narrower than one that accounted for its uncertainty.
#'
#' The posterior draws are in \code{posterior$mu$coeffs}, one column per
#' series, those of the pointwise log-likelihood in \code{posterior$loglik},
#' one column per period, and those of the forecasts in
#' \code{posterior$forecast$forecasts}, one column per series and horizon.
#'
#' @return \code{create_normalmodel()} and the \code{add_*} methods return an
#' object of class \code{"normalmodel"}, \code{use_expanding_window()} a list
#' of them of class \code{"expandingwindow"}. \code{get_forecast_errors()}
#' returns the draws of the forecast errors, \code{predict()} the forecasts as
#' an object of class \code{"bvarprd"}, which \code{\link{plot.bvarprd}} plots,
#' \code{expected_model_size()} a data frame of class \code{"modelsize"},
#' \code{write_to_hdf5()} the path of the file invisibly and \code{summary()}
#' the summary of the draws of the means, as \code{coda} computes it.
#'
#' @examples
#' # Two series, one of which moves
#' y <- ts(cbind(a = sin(1:40 / 4), b = cos(1:40 / 6)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(y, iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' model <- add_initial_values(model)
#' model <- add_posterior_coefficients(model)
#' summary(model)
#'
#' @export
create_normalmodel <- function(data, variance = NULL, iterations = 1000) {
  if (!stats::is.ts(data)) {
    stop("Argument 'data' must be a time series object, created with ts().")
  }
  y <- data
  if (is.null(dim(y))) {
    y <- stats::ts(matrix(as.numeric(y), ncol = 1), start = stats::start(data),
                   frequency = stats::frequency(data))
  }
  k <- ncol(y)
  if (is.null(colnames(y))) {
    colnames(y) <- if (k == 1) "y" else paste0("y", seq_len(k))
  }
  if (anyNA(y)) {
    stop("Argument 'data' must not contain missing values.")
  }
  if (nrow(y) < 2) {
    stop("Argument 'data' must have at least two periods.")
  }
  if (is.null(variance)) {
    variance <- apply(y, 2, stats::var)
  }
  if (!is.numeric(variance) || !length(variance) %in% c(1, k) ||
      anyNA(variance) || any(variance <= 0)) {
    stop("Argument 'variance' must be positive: one value, or one per series.")
  }
  if (length(iterations) != 1 || !is.numeric(iterations) || is.na(iterations) ||
      iterations < 1 || iterations %% 1 != 0) {
    stop("Argument 'iterations' must be a positive whole number.")
  }
  model <- list("algorithm" = "NormalMean", "type" = "normal",
                "k" = as.integer(k), "p" = 0L, "n" = 0L,
                "endogen" = colnames(y),
                "variance" = rep_len(as.numeric(variance), k),
                "iterations" = as.integer(iterations), "burnin" = 0L)
  result <- list("data" = list("train" = list("y" = y)), "model" = model)
  class(result) <- c("normalmodel", "list")
  return(result)
}

#' @rdname create_normalmodel
#' @export
print.normalmodel <- function(x, ...) {
  model <- x[["model"]]
  y <- x[["data"]][["train"]][["y"]]
  cat("Normal model of the mean with known variance of", model[["k"]], "series,",
      nrow(y), "periods\n")
  if (!is.null(x[["posterior"]][["mu"]])) {
    cat(nrow(x[["posterior"]][["mu"]][["coeffs"]]), "posterior draws\n")
  }
  return(invisible(x))
}

#' @rdname create_normalmodel
#' @export
add_priors.normalmodel <- function(object, mean = 0, prior_variance = 100, ...) {
  if (length(mean) != 1 || !is.numeric(mean) || is.na(mean)) {
    stop("Argument 'mean' must be a single number.")
  }
  if (length(prior_variance) != 1 || !is.numeric(prior_variance) ||
      is.na(prior_variance) || prior_variance <= 0) {
    stop("Argument 'prior_variance' must be a single positive number.")
  }
  object[["priors"]] <- list("mu" = list("mean" = mean, "variance" = prior_variance))
  return(object)
}

#' @rdname create_normalmodel
#' @export
add_initial_values.normalmodel <- function(object, ...) {
  # The draws are independent, so there is nothing to start from but the seed.
  if (is.null(object[["model"]][["seed"]])) {
    object[["model"]][["seed"]] <- .draw_model_seed()
  }
  return(object)
}

#' @rdname create_normalmodel
#' @export
add_seed.normalmodel <- function(object, seed, ...) {
  object[["model"]][["seed"]] <- .check_seed(seed)
  return(object)
}

#' @rdname create_normalmodel
#' @export
add_posterior_coefficients.normalmodel <- function(object, chains = 1, ...) {
  if (is.null(object[["priors"]][["mu"]])) {
    stop("Object does not contain priors. Use add_priors() first.")
  }
  if (length(chains) != 1 || !is.numeric(chains) || is.na(chains) ||
      chains < 1 || chains %% 1 != 0) {
    stop("Argument 'chains' must be a positive whole number.")
  }
  y <- object[["data"]][["train"]][["y"]]
  variance <- object[["model"]][["variance"]]
  prior <- object[["priors"]][["mu"]]
  post_variance <- 1 / (1 / prior[["variance"]] + nrow(y) / variance)
  post_mean <- post_variance * (prior[["mean"]] / prior[["variance"]] + colSums(y) / variance)
  n <- object[["model"]][["iterations"]] * as.integer(chains)
  k <- object[["model"]][["k"]]
  draws <- .with_model_seed(object[["model"]][["seed"]],
                            matrix(stats::rnorm(n * k), n, k))
  draws <- sweep(sweep(draws, 2, sqrt(post_variance), "*"), 2, post_mean, "+")
  colnames(draws) <- object[["model"]][["endogen"]]
  object <- .drop_normalmodel_posterior(object)
  object[["posterior"]] <- list("mu" = list("coeffs" = coda::mcmc(draws)))
  object[["model"]][["chains"]] <- as.integer(chains)
  return(object)
}

#' @rdname create_normalmodel
#' @export
add_posterior_loglik.normalmodel <- function(object, ...) {
  mu <- .normalmodel_draws(object)
  y <- object[["data"]][["train"]][["y"]]
  object[["posterior"]][["loglik"]] <- .mcmc_like(.normal_density(mu, y, object), mu)
  return(object)
}

#' @rdname create_normalmodel
#' @export
prepare_forecast_input.normalmodel <- function(object, n_ahead = 4, ...) {
  if (length(n_ahead) != 1 || !is.numeric(n_ahead) || is.na(n_ahead) ||
      n_ahead < 1 || n_ahead %% 1 != 0) {
    stop("Argument 'n_ahead' must be a positive whole number.")
  }
  tsp_y <- stats::tsp(object[["data"]][["train"]][["y"]])
  return(list("h" = as.integer(n_ahead), "start" = tsp_y[2] + 1 / tsp_y[3]))
}

#' @rdname create_normalmodel
#' @export
add_forecast_input.normalmodel <- function(object, n_ahead = 4, ...) {
  input <- prepare_forecast_input(object, n_ahead = n_ahead)
  object[["model"]][["h"]] <- input[["h"]]
  object[["model"]][["forecast_start"]] <- input[["start"]]
  if (!is.null(object[["posterior"]])) {
    object[["posterior"]][["forecast"]] <- NULL
  }
  object[["data"]][["test"]] <- NULL
  return(object)
}

#' @rdname create_normalmodel
#' @export
add_posterior_forecasts.normalmodel <- function(object, ...) {
  mu <- .normalmodel_draws(object)
  h <- object[["model"]][["h"]]
  if (is.null(h)) {
    stop("Object does not contain forecast input. Use add_forecast_input() first.")
  }
  k <- object[["model"]][["k"]]
  n <- nrow(mu)
  shocks <- .with_stage_seed(object, "forecast", matrix(stats::rnorm(n * k * h), n, k * h))
  sd <- rep(sqrt(object[["model"]][["variance"]]), h)
  forecasts <- shocks * rep(sd, each = n) + mu[, rep(seq_len(k), h), drop = FALSE]
  colnames(forecasts) <- paste0(object[["model"]][["endogen"]], ".h", rep(seq_len(h), each = k))
  object[["posterior"]][["forecast"]] <- list("forecasts" = .mcmc_like(forecasts, mu))
  return(object)
}

#' @rdname create_normalmodel
#' @export
predict.normalmodel <- function(object, ...) {
  forecasts <- object[["posterior"]][["forecast"]][["forecasts"]]
  if (is.null(forecasts)) {
    stop("Object does not contain forecasts. Use add_forecast_input() and ",
         "add_posterior_forecasts() first.")
  }
  y <- object[["data"]][["train"]][["y"]]
  k <- object[["model"]][["k"]]
  h <- object[["model"]][["h"]]
  forecasts <- .draws_matrix(forecasts)
  times <- object[["model"]][["forecast_start"]] + (seq_len(h) - 1) / stats::frequency(y)
  # One period per row, one series per column and one draw per slice, the
  # layout plot() of a 'bvarprd' object reads.
  fcst <- array(NA_real_, dim = c(h, k, nrow(forecasts)),
                dimnames = list(as.character(times), colnames(y), NULL))
  for (i in seq_len(nrow(forecasts))) {
    fcst[, , i] <- matrix(forecasts[i, ], h, k, byrow = TRUE)
  }
  attr(fcst, "tsp") <- c(min(times), max(times), stats::frequency(y))
  result <- list("fcst" = fcst, "y" = y)
  class(result) <- c("bvarprd", "list")
  return(result)
}

#' @rdname create_normalmodel
#' @export
add_forecast_errors.normalmodel <- function(object, test_sample = NULL, ...) {
  forecasts <- object[["posterior"]][["forecast"]][["forecasts"]]
  if (is.null(forecasts)) {
    stop("Object does not contain forecasts. Use add_posterior_forecasts() first.")
  }
  realised <- .normalmodel_realised(object, test_sample)
  if (is.null(realised)) {
    return(object)
  }
  forecasts <- .draws_matrix(forecasts)
  h <- nrow(realised)
  k <- object[["model"]][["k"]]
  errors <- matrix(as.numeric(t(realised)), nrow(forecasts), h * k, byrow = TRUE) -
    forecasts[, seq_len(h * k), drop = FALSE]
  object[["data"]][["test"]][["y"]] <- realised
  object[["posterior"]][["forecast"]][["errors"]] <- .mcmc_like(errors, .normalmodel_draws(object))
  return(object)
}

#' @rdname create_normalmodel
#' @export
get_forecast_errors.normalmodel <- function(object, ...) {
  return(object[["posterior"]][["forecast"]][["errors"]])
}

#' @rdname create_normalmodel
#' @export
add_predictive_loglik.normalmodel <- function(object, test_sample = NULL, ...) {
  mu <- .normalmodel_draws(object)
  if (is.null(object[["model"]][["h"]])) {
    stop("Object does not contain forecast input. Use add_forecast_input() first.")
  }
  realised <- .normalmodel_realised(object, test_sample)
  if (is.null(realised)) {
    return(object)
  }
  object[["data"]][["test"]][["y"]] <- realised
  # The density of each horizon given each draw of the means, the draws of the
  # predictive density that selection_criteria() averages over.
  object[["posterior"]][["forecast"]][["loglik"]] <-
    .mcmc_like(.normal_density(mu, realised, object), mu)
  return(object)
}

#' @rdname create_normalmodel
#' @export
use_expanding_window.normalmodel <- function(object, start, ...) {
  y <- object[["data"]][["train"]][["y"]]
  time_y <- as.numeric(stats::time(y))
  eps <- getOption("ts.eps")
  first <- which(time_y >= start - eps)[1]
  if (is.na(first) || first < 3) {
    stop("Argument 'start' must leave at least two periods before the first ",
         "forecast and lie within the sample.")
  }
  object <- .drop_normalmodel_posterior(object)
  ends <- (first - 1):length(time_y)
  result <- lapply(seq_along(ends), function(i) {
    window_i <- object
    window_i[["data"]][["train"]][["y"]] <- stats::window(y, end = time_y[ends[i]])
    if (!is.null(window_i[["model"]][["seed"]])) {
      window_i[["model"]][["seed"]] <- .offset_seed(window_i[["model"]][["seed"]], i - 1)
    }
    window_i
  })
  class(result) <- c("expandingwindow", "list")
  return(result)
}

#' @rdname create_normalmodel
#' @export
window.normalmodel <- function(x, start = NULL, end = NULL, ...) {
  x[["data"]][["train"]][["y"]] <- stats::window(x[["data"]][["train"]][["y"]],
                                                 start = start, end = end)
  # Draws of a different sample would no longer belong to the model.
  return(.drop_normalmodel_posterior(x))
}

#' @rdname create_normalmodel
#' @export
thin.normalmodel <- function(x, thin = 10, ...) {
  thin_draws <- function(draws) {
    if (is.null(draws)) {
      return(NULL)
    }
    mcpar <- coda::mcpar(draws)
    keep <- seq(1, coda::niter(draws), by = thin)
    coda::mcmc(.draws_matrix(draws)[keep, , drop = FALSE], start = mcpar[1],
               thin = mcpar[3] * thin)
  }
  posterior <- x[["posterior"]]
  posterior[["mu"]][["coeffs"]] <- thin_draws(posterior[["mu"]][["coeffs"]])
  posterior[["loglik"]] <- thin_draws(posterior[["loglik"]])
  for (i in names(posterior[["forecast"]])) {
    posterior[["forecast"]][[i]] <- thin_draws(posterior[["forecast"]][[i]])
  }
  x[["posterior"]] <- posterior
  return(x)
}

#' @rdname create_normalmodel
#' @export
summary.normalmodel <- function(object, ...) {
  return(summary(.normalmodel_draws(object, mcmc = TRUE), ...))
}

#' @rdname create_normalmodel
#' @export
plot.normalmodel <- function(x, ...) {
  plot(.normalmodel_draws(x, mcmc = TRUE), ...)
  return(invisible(x))
}

#' @rdname create_normalmodel
#' @export
get_model_specifications.normalmodel <- function(object, ...) {
  # From 'model' alone, which is all that selection_criteria() keeps.
  model <- object[["model"]]
  return(data.frame(algorithm = model[["algorithm"]],
                    k = model[["k"]],
                    variance = paste(format(signif(model[["variance"]], 3)), collapse = ", ")))
}

#' @rdname create_normalmodel
#' @export
expected_model_size.normalmodel <- function(object, chains = NULL, ...) {
  model <- object[["model"]]
  if (is.null(chains)) {
    chains <- if (is.null(model[["chains"]])) 1 else model[["chains"]]
  }
  n <- as.numeric(model[["iterations"]]) * chains
  tt <- nrow(object[["data"]][["train"]][["y"]])
  element <- c("posterior$mu$coeffs", "posterior$loglik")
  step <- c("add_posterior_coefficients", "add_posterior_loglik")
  columns <- c(model[["k"]], tt)
  if (!is.null(model[["h"]])) {
    element <- c(element, "posterior$forecast$forecasts")
    step <- c(step, "add_posterior_forecasts")
    columns <- c(columns, model[["k"]] * model[["h"]])
  }
  result <- data.frame(element = c("data, priors and initial values", element),
                       step = c("", step),
                       draws = c(NA, rep(n, length(element))),
                       columns = c(NA, columns),
                       bytes = c(as.numeric(utils::object.size(object[names(object) != "posterior"])),
                                 8 * n * columns),
                       stringsAsFactors = FALSE)
  class(result) <- c("modelsize", "data.frame")
  return(result)
}

#' @rdname create_normalmodel
#' @export
write_to_hdf5.normalmodel <- function(object, filename, group = "", ...) {
  model <- object[["model"]]
  model[["rclass"]] <- class(object)
  model[["rpackage"]] <- "bayests"
  tree <- list("model" = list(".attributes" = model),
               "data" = object[["data"]],
               "priors" = object[["priors"]],
               "posterior" = object[["posterior"]])
  tree <- tree[!vapply(tree, is.null, logical(1))]
  return(invisible(write_bayests_tree(tree, filename = filename, group = group)))
}

#' @rdname create_normalmodel
#' @export
from_bayests_tree.normalmodel <- function(tree, ...) {
  model <- tree[["model"]][[".attributes"]]
  model[["rclass"]] <- NULL
  model[["rpackage"]] <- NULL
  result <- list("data" = tree[["data"]], "model" = model,
                 "priors" = tree[["priors"]], "posterior" = tree[["posterior"]])
  result <- result[!vapply(result, is.null, logical(1))]
  # A model file keeps the draws but not their column names.
  endogen <- model[["endogen"]]
  if (!is.null(result[["posterior"]][["mu"]][["coeffs"]])) {
    colnames(result[["posterior"]][["mu"]][["coeffs"]]) <- endogen
  }
  for (i in c("forecasts", "errors")) {
    draws <- result[["posterior"]][["forecast"]][[i]]
    if (!is.null(draws)) {
      h <- ncol(draws) / length(endogen)
      colnames(result[["posterior"]][["forecast"]][[i]]) <-
        paste0(endogen, ".h", rep(seq_len(h), each = length(endogen)))
    }
  }
  class(result) <- c("normalmodel", "list")
  return(result)
}

# The draws of the means, as a plain matrix or as the 'mcmc' object they are
# stored as.
.normalmodel_draws <- function(object, mcmc = FALSE) {
  draws <- object[["posterior"]][["mu"]][["coeffs"]]
  if (is.null(draws)) {
    stop("Object does not contain posterior draws. Use add_posterior_coefficients() first.")
  }
  if (mcmc) {
    return(draws)
  }
  return(.draws_matrix(draws))
}

# Draws of the log density of each row of 'y' given each draw of the means: one
# row per draw, one column per period.
.normal_density <- function(mu, y, object) {
  y <- as.matrix(y)
  sd <- sqrt(object[["model"]][["variance"]])
  result <- matrix(0, nrow(mu), nrow(y))
  for (i in seq_len(ncol(y))) {
    result <- result + stats::dnorm(matrix(y[, i], nrow(mu), nrow(y), byrow = TRUE),
                                    mu[, i], sd[i], log = TRUE)
  }
  return(result)
}

# 'x' as an 'mcmc' object with the iteration numbers of 'reference'.
.mcmc_like <- function(x, reference) {
  mcpar <- coda::mcpar(coda::as.mcmc(reference))
  return(coda::mcmc(x, start = mcpar[1], thin = mcpar[3]))
}

# The realised values of the forecast periods: those of 'test_sample' the
# forecast covers, or the stored ones. NULL when none has been realised.
.normalmodel_realised <- function(object, test_sample) {
  if (is.null(test_sample)) {
    return(object[["data"]][["test"]][["y"]])
  }
  if (!stats::is.ts(test_sample)) {
    stop("Argument 'test_sample' must be a time series object.")
  }
  freq <- stats::frequency(object[["data"]][["train"]][["y"]])
  start <- object[["model"]][["forecast_start"]]
  end <- start + (object[["model"]][["h"]] - 1) / freq
  test_end <- stats::tsp(test_sample)[2]
  if (test_end < start - getOption("ts.eps")) {
    return(NULL)
  }
  realised <- stats::window(test_sample, start = start, end = min(end, test_end))
  if (is.null(dim(realised))) {
    realised <- stats::ts(matrix(as.numeric(realised), ncol = 1),
                          start = stats::start(realised), frequency = freq)
  }
  if (ncol(realised) != object[["model"]][["k"]]) {
    stop("Argument 'test_sample' must hold one column per series of the model.")
  }
  colnames(realised) <- object[["model"]][["endogen"]]
  return(realised)
}

.drop_normalmodel_posterior <- function(object) {
  object[["posterior"]] <- NULL
  object[["data"]][["test"]] <- NULL
  object[["model"]][["chains"]] <- NULL
  return(object)
}
