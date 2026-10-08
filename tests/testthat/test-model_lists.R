# The methods of 'modellist' and 'expandingwindow': each applies a step to
# every model it holds, through whatever method the model's package defines.

test_window <- function(n) {
  structure(lapply(n, test_model), class = c("expandingwindow", "list"))
}

test_that("a step reaches every model of a list with its arguments", {
  models <- add_priors(test_modellist(1:3), scale = 5)

  expect_s3_class(models, "modellist")
  for (model in models) {
    expect_identical(model[["priors"]][["a"]][["scale"]], 5)
  }

  window <- add_priors(test_window(1:2), scale = 2)
  expect_s3_class(window, "expandingwindow")
  expect_identical(window[[2]][["priors"]][["a"]][["scale"]], 2)
})

test_that("one seed numbers the models of nested lists without repeating", {
  models <- structure(list(test_model(1L), test_window(2:3), test_model(4L)),
                      class = c("modellist", "list"))

  seeded <- add_seed(models, 100)

  expect_identical(seeded[[1]][["model"]][["seed"]], 100L)
  expect_identical(seeded[[2]][[1]][["model"]][["seed"]], 101L)
  expect_identical(seeded[[2]][[2]][["model"]][["seed"]], 102L)
  expect_identical(seeded[[3]][["model"]][["seed"]], 103L)
})

test_that("a seed is a whole number in the range of an integer", {
  expect_identical(bayests::.check_seed(20260916), 20260916L)
  expect_error(bayests::.check_seed(-1), "whole number")
  expect_error(bayests::.check_seed(1.5), "whole number")
  expect_error(bayests::.check_seed(c(1, 2)), "whole number")
  expect_identical(bayests::.offset_seed(.Machine$integer.max, 1), 0L)
})

test_that("the posterior of every model is drawn and the list keeps its class", {
  models <- add_posterior_coefficients(add_seed(test_modellist(1:2), 7))

  expect_s3_class(models, "modellist")
  expect_true(coda::is.mcmc(models[[1]][["posterior"]][["a"]][["coeffs"]]))
  # Different seeds, different draws.
  expect_false(identical(models[[1]][["posterior"]][["a"]][["coeffs"]],
                         models[[2]][["posterior"]][["a"]][["coeffs"]]))
})

test_that("thinning reaches the models of a list", {
  models <- thin(add_posterior_coefficients(test_modellist(1:2)), thin = 4)

  expect_identical(coda::niter(models[[2]][["posterior"]][["a"]][["coeffs"]]), 10L)
})

test_that("combine_models() splices lists and keeps windows whole", {
  combined <- combine_models(test_model(1L), test_modellist(2:3), test_window(4:5))

  expect_s3_class(combined, "modellist")
  expect_length(combined, 4)
  expect_identical(combined[[3]][["model"]][["n"]], 3L)
  expect_s3_class(combined[[4]], "expandingwindow")
  expect_error(combine_models(test_model(), 1:3), "Argument 2")
})

test_that("a step without a method names the class it found none for", {
  plain <- structure(list("model" = list()), class = c("othermodel", "list"))

  expect_error(expected_model_size(plain), "class 'othermodel'")
  expect_error(time_variation_test(plain), "class 'othermodel'")
})
