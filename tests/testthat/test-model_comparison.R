# selection_criteria(), choose_best_model() and chain_diagnostics() on the
# draws of any model.

test_that("the criteria of a model are computed from its pointwise draws", {
  model <- add_posterior_coefficients(test_model(n = 1L))

  criteria <- selection_criteria(model)

  expect_s3_class(criteria, "selcrit")
  expect_named(criteria, c("model", "LL", "WAIC", "LOOIC"), ignore.order = TRUE)
  total <- rowSums(unclass(model[["posterior"]][["loglik"]]))
  expect_equal(criteria[["LL"]][["mean"]], mean(total))
  expect_identical(attr(criteria, "ci"), c("2.5%", "97.5%"))
  expect_error(selection_criteria(model, ci = 2), "between 0 and 1")
  expect_error(selection_criteria(test_model()), "posterior draws of the log-likelihood")
})

test_that("a scored forecast gives a predictive likelihood", {
  set.seed(3)
  model <- list("model" = list("k" = 2),
                "posterior" = list("forecast" = list("loglik" = matrix(stats::rnorm(150, -3), 50, 3))))

  criteria <- selection_criteria(model)

  expect_false(is.null(criteria[["LPL"]]))
  expect_null(criteria[["LL"]])
})

test_that("the best model of a list is the one the criterion favours", {
  # The test model's log-likelihood falls with n, so the first model fits best.
  models <- add_posterior_coefficients(add_seed(test_modellist(1:3), 11))

  criteria <- selection_criteria(models)

  expect_s3_class(criteria, "selcritlist")
  expect_identical(choose_best_model(criteria, criterion = "LL"), 1L)
  expect_identical(choose_best_model(criteria, criterion = "WAIC"), 1L)
  expect_error(choose_best_model(criteria, criterion = "FE"), "must be one of")
  expect_output(print(criteria), "In-sample")
})

test_that("several chains are compared parameter by parameter", {
  model <- add_posterior_coefficients(test_model(), chains = 2L)

  diagnostics <- chain_diagnostics(model)

  expect_s3_class(diagnostics, "data.frame")
  expect_setequal(diagnostics[["parameter"]], c("a1", "a2"))
  expect_true(all(diagnostics[["rhat"]] < 1.2))
  expect_error(chain_diagnostics(add_posterior_coefficients(test_model())), "single chain")
  expect_error(chain_diagnostics(test_modellist()), "model object")
})
