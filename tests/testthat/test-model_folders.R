# open_models(), map_models() and the steps of a 'modelfolder': models kept in
# files and worked on one at a time.

write_test_folder <- function(n = 1:3) {
  folder <- temp_model_dir()
  write_to_hdf5(test_modellist(n), folder = folder)
  folder
}

test_that("the manifest lists every model with its algorithm and package", {
  folder <- write_test_folder(1:3)

  x <- open_models(folder)

  expect_s3_class(x, "modelfolder")
  manifest <- x[["manifest"]]
  expect_identical(nrow(manifest), 3L)
  expect_identical(unique(manifest[["algorithm"]]), "TestFactor")
  # The file records no package, and the algorithm belongs to none.
  expect_true(all(is.na(manifest[["package"]])))
  expect_identical(sort(manifest[["n"]]), 1:3)
  expect_length(model_files(x), 3)
  expect_output(print(x), "3 models")
})

test_that("a model's package comes from the file, or from its algorithm", {
  expect_identical(bayests:::.model_package(list("algorithm" = "VarNormalGamma")),
                   "bvartools")
  expect_identical(bayests:::.model_package(list("algorithm" = "VarNormalGamma",
                                                 "rpackage" = "mypkg")),
                   "mypkg")
  expect_identical(bayests:::.model_package(list("algorithm" = "Example")),
                   NA_character_)
})

test_that("a step applied to a folder is written back into each file", {
  folder <- write_test_folder(1:2)
  x <- open_models(folder)

  x <- add_priors(x, scale = 3)

  back <- read_models_from_folder(folder)
  for (model in back) {
    expect_identical(as.numeric(model[["priors"]][["a"]][["scale"]]), 3)
  }
})

test_that("map_models() returns what the function returns when not writing", {
  folder <- write_test_folder(1:3)
  x <- open_models(folder)

  n <- map_models(x, function(model) model[["model"]][["n"]], write = FALSE)

  expect_identical(sort(unlist(n, use.names = FALSE)), 1:3)
  expect_identical(names(n), x[["manifest"]][["model"]])
  expect_error(map_models(list(), identity), "class 'modelfolder'")
})

test_that("a folder's seeds number its models from the one given", {
  folder <- write_test_folder(1:3)
  x <- add_seed(open_models(folder), 50)

  seeds <- map_models(x, function(model) model[["model"]][["seed"]], write = FALSE)
  expect_setequal(unlist(seeds, use.names = FALSE), 50:52)
})
