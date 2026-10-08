# The functions model packages call from their methods are exported, so that
# none of them has to reach into this namespace with ':::'.

test_that("the developer functions are exported", {
  exported <- getNamespaceExports("bayests")
  expect_true(all(c(".check_seed", ".draw_model_seed", ".offset_seed",
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
