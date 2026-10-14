# The conjugate normal model of create_normalmodel(): that its draws come from
# the posterior it is documented to have, that its log-likelihoods, forecasts
# and errors are the quantities their names promise, and that it survives a
# model file. The lists of models it is used with are tested elsewhere.

normal_data <- function() {
  .with_model_seed(1, stats::ts(cbind(a = stats::rnorm(40, 1), b = stats::rnorm(40, -1)),
                                start = c(2000, 1), frequency = 4))
}

fitted_normal <- function(y = normal_data(), iterations = 200, ...) {
  model <- create_normalmodel(y, iterations = iterations, ...)
  model <- add_priors(model, mean = 0, prior_variance = 10)
  model <- add_seed(model, 42)
  add_posterior_coefficients(model)
}

test_that("the draws come from the conjugate posterior of the means", {
  y <- normal_data()
  model <- fitted_normal(y, iterations = 20000, variance = c(1, 4))
  post_variance <- 1 / (1 / 10 + 40 / c(1, 4))
  post_mean <- post_variance * colSums(y) / c(1, 4)
  draws <- unclass(model[["posterior"]][["mu"]][["coeffs"]])

  expect_equal(unname(colMeans(draws)), unname(post_mean), tolerance = 0.01)
  expect_equal(unname(apply(draws, 2, stats::var)), post_variance, tolerance = 0.03)
  expect_equal(colnames(draws), c("a", "b"))
})

test_that("the sample variance is taken as known when none is given", {
  y <- normal_data()
  model <- create_normalmodel(y)

  expect_equal(model[["model"]][["variance"]], unname(apply(y, 2, stats::var)))
  expect_equal(create_normalmodel(y, variance = 2)[["model"]][["variance"]], c(2, 2))
})

test_that("a seed makes the draws reproducible and two chains are stacked", {
  expect_identical(fitted_normal()[["posterior"]], fitted_normal()[["posterior"]])

  model <- add_posterior_coefficients(fitted_normal(), chains = 2)
  expect_equal(nrow(model[["posterior"]][["mu"]][["coeffs"]]), 400)
  expect_equal(model[["model"]][["chains"]], 2L)
  expect_s3_class(chain_diagnostics(model), "data.frame")
})

test_that("the pointwise log-likelihood is the normal density of each period", {
  y <- normal_data()
  model <- add_posterior_loglik(fitted_normal(y, variance = c(1, 4)))
  mu <- unclass(model[["posterior"]][["mu"]][["coeffs"]])
  expected <- stats::dnorm(y[7, "a"], mu[3, "a"], 1, log = TRUE) +
    stats::dnorm(y[7, "b"], mu[3, "b"], 2, log = TRUE)

  expect_equal(dim(model[["posterior"]][["loglik"]]), c(200L, 40L))
  expect_equal(unclass(model[["posterior"]][["loglik"]])[3, 7], unname(expected))
})

test_that("forecast errors are the realised values less the forecasts", {
  y <- normal_data()
  model <- fitted_normal(stats::window(y, end = c(2008, 4)))
  model <- add_forecast_input(model, n_ahead = 4)
  model <- add_posterior_forecasts(model)
  model <- add_forecast_errors(model, test_sample = y)
  forecasts <- unclass(model[["posterior"]][["forecast"]][["forecasts"]])
  errors <- unclass(model[["posterior"]][["forecast"]][["errors"]])

  expect_equal(model[["model"]][["forecast_start"]], 2009)
  expect_equal(colnames(forecasts)[1:3], c("a.h1", "b.h1", "a.h2"))
  expect_equal(unname(errors[5, "b.h2"]), unname(y[38, "b"] - forecasts[5, "b.h2"]))
  expect_equal(nrow(model[["data"]][["test"]][["y"]]), 4)
})

test_that("predict() lays out the forecast draws for plot()", {
  model <- add_forecast_input(fitted_normal(stats::window(normal_data(), end = c(2008, 4))),
                              n_ahead = 3)
  model <- add_posterior_forecasts(model)
  forecasts <- unclass(model[["posterior"]][["forecast"]][["forecasts"]])
  pred <- predict(model)

  expect_s3_class(pred, "bvarprd")
  expect_equal(dim(pred[["fcst"]]), c(3L, 2L, 200L))
  expect_equal(pred[["fcst"]][2, "b", 11], unname(forecasts[11, "b.h2"]))
  expect_equal(attr(pred[["fcst"]], "tsp"), c(2009, 2009.5, 4))
  expect_equal(names(forecast_plot_series(pred)), c("a", "b"))
  expect_error(predict(fitted_normal()), "add_posterior_forecasts")
})

test_that("a horizon that is not yet realised is not scored", {
  y <- normal_data()
  model <- add_forecast_input(fitted_normal(stats::window(y, end = c(2009, 2))), n_ahead = 4)
  model <- add_posterior_forecasts(model)
  model <- add_predictive_loglik(model, test_sample = y)
  mu <- unclass(model[["posterior"]][["mu"]][["coeffs"]])
  variance <- model[["model"]][["variance"]]
  expected <- stats::dnorm(y[40, "a"], mu[9, "a"], sqrt(variance[1]), log = TRUE) +
    stats::dnorm(y[40, "b"], mu[9, "b"], sqrt(variance[2]), log = TRUE)

  # Two of the four horizons lie within the sample.
  expect_equal(dim(model[["posterior"]][["forecast"]][["loglik"]]), c(200L, 2L))
  expect_equal(unclass(model[["posterior"]][["forecast"]][["loglik"]])[9, 2], unname(expected))
  expect_false(is.null(selection_criteria(model)[["LPL"]]))

  ahead <- add_forecast_input(fitted_normal(y), n_ahead = 2)
  ahead <- add_posterior_forecasts(ahead)
  expect_null(add_forecast_errors(ahead, test_sample = y)[["posterior"]][["forecast"]][["errors"]])
})

test_that("an expanding window holds one model per forecast origin", {
  model <- add_seed(create_normalmodel(normal_data()), 7)
  windows <- use_expanding_window(model, start = 2008)
  ends <- vapply(windows, function(x) stats::tsp(x[["data"]][["train"]][["y"]])[2], numeric(1))

  expect_s3_class(windows, "expandingwindow")
  expect_equal(ends, seq(2007.75, 2009.75, by = 0.25))
  expect_equal(windows[[3]][["model"]][["seed"]], 9L)
})

test_that("window() and thin() keep the model consistent", {
  model <- add_posterior_loglik(fitted_normal())
  shorter <- window(model, start = c(2005, 1))
  thinned <- thin(model, thin = 5)

  expect_equal(nrow(shorter[["data"]][["train"]][["y"]]), 20)
  expect_null(shorter[["posterior"]])
  expect_equal(nrow(thinned[["posterior"]][["mu"]][["coeffs"]]), 40)
  expect_equal(nrow(thinned[["posterior"]][["loglik"]]), 40)
  expect_equal(coda::thin(thinned[["posterior"]][["loglik"]]), 5)
})

test_that("a model file gives back the model that was written", {
  model <- fitted_normal(stats::window(normal_data(), end = c(2008, 4)))
  model <- add_posterior_loglik(model)
  model <- add_posterior_forecasts(add_forecast_input(model, n_ahead = 2))
  model <- add_forecast_errors(model, test_sample = normal_data())
  file <- temp_h5_file()
  write_to_hdf5(model, filename = file)
  stored <- read_model_from_hdf5(file)

  expect_s3_class(stored, "normalmodel")
  expect_equal(stored[["model"]], model[["model"]])
  expect_equal(stored[["priors"]], model[["priors"]])
  expect_equal(stored[["data"]][["train"]], model[["data"]][["train"]])
  expect_equal(stored[["posterior"]][["mu"]][["coeffs"]], model[["posterior"]][["mu"]][["coeffs"]])
  expect_equal(stored[["posterior"]][["forecast"]][["errors"]],
               model[["posterior"]][["forecast"]][["errors"]])
  # The length of the chain of a model that has no error precision
  expect_equal(read_model_file(file)[["draws"]], 200L)
})

test_that("the size of a model is counted before it is estimated", {
  model <- add_priors(create_normalmodel(normal_data(), iterations = 100), prior_variance = 1)
  size <- expected_model_size(model, chains = 2)

  expect_s3_class(size, "modelsize")
  expect_equal(size[["bytes"]][size[["element"]] == "posterior$loglik"], 8 * 200 * 40)
})

test_that("a model that cannot be estimated says why", {
  y <- normal_data()
  expect_error(create_normalmodel(as.numeric(y[, 1])), "time series")
  expect_error(create_normalmodel(y, variance = c(1, 2, 3)), "variance")
  expect_error(add_posterior_coefficients(create_normalmodel(y)), "add_priors")
  expect_error(add_posterior_forecasts(fitted_normal(y)), "add_forecast_input")
})
