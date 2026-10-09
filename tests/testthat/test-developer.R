# The functions model packages call from their methods are exported, so that
# none of them has to reach into this namespace with ':::'.

test_that("the developer functions are exported", {
  exported <- getNamespaceExports("bayests")
  expect_true(all(c(".check_seed", ".draw_model_seed", ".offset_seed",
                    ".with_model_seed", ".stage_seed", ".with_stage_seed",
                    ".draws_matrix", ".hdf5_write_model", ".hdf5_group",
                    ".hdf5_write", ".hdf5_write_attr", ".hdf5_draws_attrs",
                    ".hdf5_series_attrs", ".hdf5_read_matrix", ".hdf5_read_value",
                    ".hdf5_restore_class", ".read_posterior_block",
                    ".log_mean_exp", ".nse_log_mean_exp", ".lpl_entry",
                    ".loo_data_frame", ".waic_data_frame") %in% exported))
})

test_that("draws come back as a plain matrix with their names", {
  draws <- coda::mcmc(matrix(1:6, 3, 2, dimnames = list(NULL, c("x", "y"))))
  m <- bayests::.draws_matrix(draws)

  expect_false(coda::is.mcmc(m))
  expect_identical(colnames(m), c("x", "y"))
  expect_identical(dim(bayests::.draws_matrix(coda::mcmc(1:4))), c(4L, 1L))
})

test_that("a log mean of exponentials is computed without overflow", {
  x <- c(1000, 1000 + log(3))
  expect_equal(bayests::.log_mean_exp(x), 1000 + log(2))
})

test_that("an expression is evaluated under a model's seed and the generator put back", {
  set.seed(1)
  state <- get(".Random.seed", envir = globalenv())
  first <- .with_model_seed(7L, stats::runif(3))
  expect_identical(get(".Random.seed", envir = globalenv()), state)
  expect_identical(.with_model_seed(7L, stats::runif(3)), first)

  # The same draws under another kind of generator, which is put back too.
  old <- RNGkind("L'Ecuyer-CMRG")
  other <- .with_model_seed(7L, stats::runif(3))
  kind_after <- RNGkind()[1]
  RNGkind(old[1], old[2], old[3])
  expect_identical(other, first)
  expect_identical(kind_after, "L'Ecuyer-CMRG")

  # Without a seed, from the generator as it stands.
  set.seed(2)
  unseeded <- .with_model_seed(NULL, stats::runif(3))
  set.seed(2)
  expect_identical(unseeded, stats::runif(3))
})

test_that("each stage after the coefficients has a seed of its own", {
  stages <- c("forecast", "predictive", "structural")
  seeds <- vapply(stages, function(stage) .stage_seed(7L, stage), integer(1))
  expect_false(anyDuplicated(c(7L, seeds)) > 0)
  # Nor those of the model numbered next to it in a list.
  expect_false(any(seeds == vapply(stages, function(stage) .stage_seed(8L, stage), integer(1))))
  expect_null(.stage_seed(NULL, "forecast"))
  expect_error(.stage_seed(7L, "coefficients"))

  model <- list(model = list(seed = 7L))
  expect_identical(.with_stage_seed(model, "forecast", stats::runif(2)),
                   .with_model_seed(seeds[["forecast"]], stats::runif(2)))
})
