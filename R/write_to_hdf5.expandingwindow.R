#' Export to HDF5 File
#' 
#' Exports the content of an object of class 'expandingwindow' to multiple HDF5 file.
#' 
#' @param object an object of class 'expandingwindow'.
#' @param folder path to the folder where the individual models in argument
#' \code{object} should be saved.
#' @param ... further arguments passed to or from other methods.
#' 
#' 
#' @return The paths of the written files, invisibly, one per window, in the
#' subfolder of \code{folder} created for the exercise.
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
#' folder <- file.path(tempdir(), "bayests-example-windows")
#' unlink(folder, recursive = TRUE)
#' dir.create(folder)
#' write_to_hdf5(windows, folder = folder)
#' list.files(folder, recursive = TRUE)
#'
#' @export
write_to_hdf5.expandingwindow <- function(object, folder, ...){
  
  if (dir.exists(folder)) {
    
    # Get model type from the first object in the list
    model_type <- object[[1]][["model"]][["algorithm"]]
    
    # Get list of files in the folder
    dir_names <- list.dirs(folder, recursive = FALSE, full.names = FALSE)
    
    model_name <- paste0("01-ExpWind-", model_type)
    model_name <- paste0(model_name, paste0("-varsel=", object[[1]][["model"]][["varsel"]]))
    model_name <- paste0(model_name, paste0("-n=", sprintf(paste0("%0", 2, "d"), object[[1]][["model"]][["n"]])))
    model_name <- paste0(model_name, paste0("-p=", sprintf(paste0("%0", 2, "d"), object[[1]][["model"]][["p"]])))
    model_name <- paste0(model_name, paste0("-m=", sprintf(paste0("%0", 2, "d"), object[[1]][["model"]][["m"]])))
    model_name <- paste0(model_name, paste0("-s=", sprintf(paste0("%0", 2, "d"), object[[1]][["model"]][["s"]])))
    
    i <- 1
    cond <- dir.exists(file.path(folder, model_name))
    if (cond) {
      while (cond) {
        substring(model_name, 1, 2) <- sprintf(paste0("%0", 2, "d"), i)
        cond <- dir.exists(file.path(folder, model_name))
        i <- i + 1
      }
    }
    
    folder <- file.path(folder, model_name)
    dir.create(folder)
    
    # Add leading zeros to file names, so that they are listed in correct order
    # in the filesystem
    n_models <- length(object)
    num_digits <- nchar(as.character(n_models))
    model_id <- sprintf(paste0("%0", num_digits, "d"), 1:n_models)
    
    paths <- character(n_models)
    for (i in 1:n_models) {
        # Which collection a model belongs to is a property of the model, not of
        # the directory it lands in. Recorded here so that a reader can restore
        # the class of the collection from the files rather than by looking for
        # "ExpWind" in a path. The per-model writer turns every element of
        # 'model' into an attribute of the model's /model group, so this needs
        # no support of its own there.
        member <- object[[i]]
        member[["model"]][["rclass_collection"]] <- class(object)

        paths[i] <- write_to_hdf5(member, filename = file.path(folder, paste0("Window-", model_id[i], ".h5")), ...)
    }

    return(invisible(paths))

  } else {
    stop("Specified 'folder' does not exist.")
  }
  
}
