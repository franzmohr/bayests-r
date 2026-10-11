# Internal helpers that the packages building on this one used to test through
# ':::' from their own suites, where a change here would break their reverse
# dependency checks. They are tested here, beside the code.

test_that("group names are normalised the way BayesTS spells them", {
  expect_equal(.normalize_hdf5_group(""), "")
  expect_equal(.normalize_hdf5_group("/"), "")
  expect_equal(.normalize_hdf5_group(NULL), "")
  expect_equal(.normalize_hdf5_group("/models/3"), "/models/3")
  expect_equal(.normalize_hdf5_group("models/3"), "/models/3")
  expect_equal(.normalize_hdf5_group("/models/3/"), "/models/3")
  # Idempotent, so a name that has been through it can go through again.
  expect_equal(.normalize_hdf5_group(.normalize_hdf5_group("models/3/")), "/models/3")

  for (bad in c("/models//3", "/models/./3", "/models/../3", "//")) {
    expect_error(.normalize_hdf5_group(bad), info = bad)
  }
  expect_error(.normalize_hdf5_group(c("a", "b")))
})

test_that("the writer's explicit types are the ones hdf5r would guess", {
  # write_bayests_tree() hands hdf5r a cached type and dataspace instead of
  # letting it guess them. The file must not change for it, so each cached type
  # is checked against the guess hdf5r makes with the string length it passes,
  # and each dataspace against guess_space().
  values <- list(character = c("VecNormalWishart", "a"), scalar_character = "none",
                 logical = TRUE, integer = 3L, integers = 1:4, double = 0.98,
                 doubles = c(1990, 2023.75, 4), matrix = matrix(1:6 * 1.0, 2))
  for (name in names(values)) {
    value <- values[[name]]
    guessed <- hdf5r::guess_dtype(value, scalar = FALSE, string_len = Inf)
    expect_equal(.hdf5_dtype(value)$to_text(), guessed$to_text(), info = name)
    expected <- hdf5r::guess_space(value, dtype = guessed, chunked = FALSE)
    expect_equal(.hdf5_attr_space(value)$get_simple_extent_dims(),
                 expected$get_simple_extent_dims(), info = name)
  }

  # What it has no type for is left to hdf5r.
  expect_null(.hdf5_dtype(factor("a")))
  expect_null(.hdf5_dtype(list(1)))
  expect_null(.hdf5_attr_space(list(1)))
})

test_that("models are found through nested lists and external forecasts are kept whole", {
  window <- structure(lapply(1:3, function(i) test_model()),
                      class = c("expandingwindow", "list"))
  models <- structure(list(window, window), class = c("modellist", "list"))
  expect_identical(.model_paths(models),
                   list(c(1L, 1L), c(1L, 2L), c(1L, 3L), c(2L, 1L), c(2L, 2L), c(2L, 3L)))

  external <- structure(list(list(), list()),
                        class = c("externalforecast", "expandingwindow", "list"))
  nested <- structure(list(window, external), class = c("modellist", "list"))
  expect_identical(.model_paths(nested), list(c(1L, 1L), c(1L, 2L), c(1L, 3L), 2L))
})

test_that("the packages whose methods the models need are found", {
  # A method another package registers for a class of its own: its environment
  # is that package's namespace.
  method <- function(object, ...) object
  environment(method) <- asNamespace("splines")
  registerS3method("add_posterior_coefficients", "bayests_internal_test_model",
                   method, envir = asNamespace("bayests"))
  other <- structure(list(), class = c("bayests_internal_test_model", "list"))

  expect_identical(.method_namespaces("add_posterior_coefficients", list(other)), "splines")
  # A generic with no method for the class needs nothing loaded.
  expect_identical(.method_namespaces("add_posterior_loglik", list(other)), character())
})

test_that("workers load the packages they are given", {
  skip_on_cran()
  loaded <- function(package) isNamespaceLoaded(package)
  environment(loaded) <- baseenv()

  # A base package that a worker has not loaded on its own, so that loading it
  # is what the test sees. Which ones a worker starts with is a matter of the
  # installation, so it is asked rather than assumed.
  plain <- .start_model_cluster(1)
  on.exit(parallel::stopCluster(plain), add = TRUE)
  candidates <- c("splines", "compiler", "grid", "tools")
  unloaded <- candidates[!vapply(candidates, function(package) {
    parallel::clusterCall(plain, loaded, package)[[1]]
  }, logical(1))]
  skip_if(length(unloaded) == 0, "a worker starts with every candidate package loaded")
  package <- unloaded[1]

  cl <- .start_model_cluster(1, packages = package)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  expect_true(parallel::clusterCall(cl, loaded, package)[[1]])
})

test_that("window() of a model list passes its further arguments to every model", {
  seen <- new.env()
  registerS3method("window", "bayests_window_test_model", function(x, start = NULL,
                                                                    end = NULL, ...) {
    seen[["extend"]] <- list(...)[["extend"]]
    x
  }, envir = asNamespace("stats"))
  model <- structure(list(), class = c("bayests_window_test_model", "list"))
  models <- structure(list(model), class = c("modellist", "list"))
  stats::window(models, start = 1, extend = TRUE)
  expect_true(isTRUE(seen[["extend"]]))
})
