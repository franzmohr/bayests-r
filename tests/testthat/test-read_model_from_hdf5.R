# read_model_from_hdf5(): which class and package a file is read as, and the
# from_hdf5() step a package can take over from the tree.

test_that("a file is read through the from_hdf5() method of its class", {
  registerS3method("from_hdf5", "groupmodel", function(x, h5_root, draws = NULL, ...) {
    structure(list("model" = hdf5r::h5attributes(h5_root[["model"]]),
                   "read_by" = "from_hdf5.groupmodel"),
              class = class(x))
  }, envir = asNamespace("bayests"))

  path <- temp_h5_file()
  write_bayests_tree(list("model" = list(".attributes" = list(
    "algorithm" = "Example", "k" = 3L, "rclass" = c("groupmodel", "list")))),
    filename = path)

  back <- read_model_from_hdf5(path)

  expect_s3_class(back, "groupmodel")
  expect_identical(back[["read_by"]], "from_hdf5.groupmodel")
  expect_identical(back[["model"]][["k"]], 3L)
})

test_that("without a from_hdf5() method the tree goes to from_bayests_tree()", {
  path <- temp_h5_file()
  model <- add_posterior_coefficients(test_model(n = 2L))
  write_to_hdf5(model, filename = path)

  back <- read_model_from_hdf5(path)

  expect_s3_class(back, "testmodel")
  # The tree keeps the draws and their chain, not their column names.
  expect_equal(unclass(back[["posterior"]][["a"]][["coeffs"]]),
               unclass(model[["posterior"]][["a"]][["coeffs"]]), ignore_attr = TRUE)
  expect_identical(coda::mcpar(back[["posterior"]][["a"]][["coeffs"]]),
                   coda::mcpar(model[["posterior"]][["a"]][["coeffs"]]))
})

test_that("a partial read hands only the draws asked for to the model's method", {
  path <- temp_h5_file()
  model <- add_posterior_coefficients(test_model())
  write_to_hdf5(model, filename = path)

  back <- read_model_from_hdf5(path, draws = c(2L, 5L))

  expect_equal(unclass(back[["posterior"]][["a"]][["coeffs"]])[, 1],
               unclass(model[["posterior"]][["a"]][["coeffs"]])[c(2, 5), 1],
               ignore_attr = TRUE)
  expect_error(read_model_from_hdf5(path, draws = 0), "at least one")
  expect_error(read_model_from_hdf5(path, draws = 1000), "Draw 1000")
})

test_that("a file of the command line is placed by the prefix of its algorithm", {
  known <- function(algorithm) bayests:::.algorithm_package(algorithm)
  expect_identical(known("VarNormalGamma")[["class"]], c("bvarmodel", "list"))
  expect_identical(known("VecKlgs2010")[["class"]], c("bvecmodel", "list"))
  expect_identical(known("DfmTvpStochvol")[["package"]], "dfmtools")
  expect_identical(known("FavarNormalWishart")[["class"]], c("favarmodel", "list"))
  expect_identical(known("UcNormalSkew")[["package"]], "ucmtools")
  expect_null(known("Example"))
  expect_null(known(NULL))
})

test_that("a file of an uninstalled package names the package to install", {
  skip_if(requireNamespace("ucmtools", quietly = TRUE), "ucmtools is installed")
  path <- temp_h5_file()
  write_bayests_tree(list("model" = list(".attributes" = list(
    "algorithm" = "UcNormalGamma", "k" = 1L))), filename = path)

  expect_error(read_model_from_hdf5(path), "Install ucmtools")
})

test_that("a file that names no class and no known algorithm is refused", {
  path <- temp_h5_file()
  write_bayests_tree(list("model" = list(".attributes" = list(
    "algorithm" = "Example"))), filename = path)

  expect_error(read_model_from_hdf5(path), "records no class")
  expect_type(read_bayests_tree(path), "list")
})

test_that("a file without a specification is refused", {
  path <- temp_h5_file()
  write_bayests_tree(list("data" = list("y" = 1:3)), filename = path)

  expect_error(read_model_from_hdf5(path), "does not contain model specification")
})
