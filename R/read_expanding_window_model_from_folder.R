#' Import Models from HDF5 Files
#' 
#' Imports model information and posterior draws from an HDF5 file.
#' 
#' @param folder Path to a folder with HDF5 files containing model data.
#'
#' @return A list of class 'expandingwindow' with one model per file in
#' \code{folder}, each read by \code{\link{read_model_from_hdf5}}, in the order
#' \code{\link[base:list.files]{list.files}} returns the files.
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(y, iterations = 200)
#'
#' # One window per forecast origin from 2007Q4 on
#' windows <- use_expanding_window(model, start = 2008)
#' windows <- add_priors(windows, mean = 0, prior_variance = 10)
#' windows <- add_initial_values(windows)
#' windows <- add_posterior_coefficients(windows)
#'
#' folder <- file.path(tempdir(), "bayests-example-read-windows")
#' unlink(folder, recursive = TRUE)
#' dir.create(folder)
#' # The windows are written to a folder of their own inside 'folder'
#' paths <- write_to_hdf5(windows, folder = folder)
#' stored <- read_expanding_window_model_from_folder(dirname(paths[1]))
#' length(stored)
#'
#' @export
read_expanding_window_model_from_folder <- function(folder) {
  
  
  dir_paths <- list.dirs(folder, recursive = FALSE)
  if (length(dir_paths) > 0) {
    stop("Specified directory contains subfolders.")
  }
  
  file_paths <- list.files(folder, full.names = TRUE)
  
  if (length(file_paths) == 0) {
    stop("No files found.")
  }

  result <- NULL
  for (i in 1:length(file_paths)) {
    result[[i]] <- read_model_from_hdf5(filename = file_paths[i])
  }
  class(result) <- c("expandingwindow", "list") 
  
  return(result)
}
