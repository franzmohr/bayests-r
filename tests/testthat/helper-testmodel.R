# A model class of a package that builds on this one, as bvartools' VARs and
# dfmtools' factor models do. Its methods are registered rather than defined
# here, because the generics dispatch from the namespace, and the methods the
# lists of models call are found there too.

local({
  ns <- asNamespace("bayests")

  registerS3method("write_to_hdf5", "testmodel", function(object, filename, group = "", ...) {
    write_bayests_tree(list(
      "model" = list(".attributes" = c(object[["model"]],
                                       list("rclass" = class(object)))),
      "data" = list("train" = list("y" = object[["data"]][["x"]])),
      "priors" = object[["priors"]],
      "posterior" = object[["posterior"]]
    ), filename = filename, group = group)
  }, envir = ns)

  registerS3method("from_bayests_tree", "testmodel", function(tree, ...) {
    model <- tree[["model"]][[".attributes"]]
    model[["rclass"]] <- NULL
    structure(list("data" = list("x" = tree[["data"]][["train"]][["y"]]),
                   "model" = model,
                   "priors" = tree[["priors"]],
                   "posterior" = tree[["posterior"]]),
              class = class(tree))
  }, envir = ns)

  registerS3method("add_priors", "testmodel", function(object, scale = 1, ...) {
    object[["priors"]] <- list("a" = list("scale" = scale))
    object
  }, envir = ns)

  registerS3method("add_initial_values", "testmodel", function(object, ...) {
    object[["initial"]] <- list("a" = 0)
    if (is.null(object[["model"]][["seed"]])) {
      object[["model"]][["seed"]] <- bayests::.draw_model_seed()
    }
    object
  }, envir = ns)

  registerS3method("add_seed", "testmodel", function(object, seed, ...) {
    object[["model"]][["seed"]] <- bayests::.check_seed(seed)
    object
  }, envir = ns)

  # Draws whose values depend only on the model's seed, two chains stacked
  # when 'chains' is two, and a pointwise log-likelihood for the criteria.
  registerS3method("add_posterior_coefficients", "testmodel", function(object, chains = 1L, ...) {
    seed <- object[["model"]][["seed"]]
    if (is.null(seed)) seed <- 1L
    old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
    on.exit(if (!is.null(old)) assign(".Random.seed", old, globalenv()))
    set.seed(seed)
    n <- 40L * chains
    a <- matrix(stats::rnorm(2 * n), n, 2, dimnames = list(NULL, c("a1", "a2")))
    object[["posterior"]] <- list(
      "a" = list("coeffs" = coda::mcmc(a)),
      "loglik" = coda::mcmc(matrix(stats::rnorm(n * 10, -1 - object[["model"]][["n"]]), n, 10)))
    object[["model"]][["chains"]] <- as.integer(chains)
    object
  }, envir = ns)

  registerS3method("thin", "testmodel", function(x, thin = 10, ...) {
    keep <- seq(1, coda::niter(x[["posterior"]][["a"]][["coeffs"]]), by = thin)
    x[["posterior"]][["a"]][["coeffs"]] <- coda::mcmc(x[["posterior"]][["a"]][["coeffs"]][keep, , drop = FALSE])
    x
  }, envir = ns)
})

test_model <- function(n = 2L) {
  x <- stats::ts(matrix(seq_len(20) / 10, 10, 2), start = c(2000, 1), frequency = 4)
  colnames(x) <- c("a", "b")
  structure(list("data" = list("x" = x),
                 "model" = list("algorithm" = "TestFactor", "k" = 2L,
                                "n" = n, "p" = 1L)),
            class = c("testmodel", "list"))
}

test_modellist <- function(n = 1:3) {
  structure(lapply(n, test_model), class = c("modellist", "list"))
}

temp_h5_file <- function() {
  tempfile(fileext = ".h5")
}

temp_model_dir <- function() {
  path <- tempfile()
  dir.create(path)
  path
}
