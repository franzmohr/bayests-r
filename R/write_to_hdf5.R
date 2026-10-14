#' Export to HDF5 File
#' 
#' A generic function used to export the content of models into HDF5 files.
#' The function invokes particular methods which depend on the class
#' of the first argument.
#' 
#' @param object an object of a class, for which a method should be called.
#' @param ... arguments passed forward to method.
#'
#' @details Before the method is called, the size of \code{object} is compared
#' with \code{options(bayests.size_warning)}, and a warning is given if it is
#' larger, since that is about the disk space the files will need. See
#' \code{\link{expected_model_size}}.
#' 
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(y, iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' model <- add_initial_values(model)
#' model <- add_posterior_coefficients(model)
#'
#' file <- file.path(tempdir(), "bayests-example-write.h5")
#' unlink(file)
#' write_to_hdf5(model, filename = file)
#' read_model_from_hdf5(file)
#'
#' @seealso Methods: \code{write_to_hdf5.bvarmodel} (\pkg{bvartools}),
#' \code{write_to_hdf5.bvecmodel} (\pkg{bvartools}),
#' \code{\link{write_to_hdf5.expandingwindow}},
#' \code{\link{write_to_hdf5.modellist}}.
#'
#' @export
write_to_hdf5 <- function (object, ...) {
  # Once per call from the outside, as in add_posterior_coefficients().
  if (!isTRUE(.size_check[["file"]])) {
    .check_file_size(object)
    .size_check[["file"]] <- TRUE
    on.exit(.size_check[["file"]] <- FALSE, add = TRUE)
  }
  UseMethod("write_to_hdf5")
}
