#' Export to HDF5 File
#' 
#' Exports the content of an object of class 'modellist' to multiple HDF5 file.
#' 
#' @param object an object of class 'modellist'.
#' @param folder path to the folder where the individual models in argument
#' \code{object} should be saved.
#' @param ... further arguments passed to or from other methods.
#' 
#' 
#' @return The paths of the written files, invisibly, one per model and, for a
#' member that is itself a collection, one per model in it.
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#' train <- window(y, end = c(2008, 4))
#'
#' # Two models that differ in the variance they take as known
#' models <- combine_models(create_normalmodel(train, variance = 1, iterations = 200),
#'                          create_normalmodel(train, variance = 4, iterations = 200))
#' models <- add_priors(models, mean = 0, prior_variance = 10)
#'
#' folder <- file.path(tempdir(), "bayests-example-list")
#' unlink(folder, recursive = TRUE)
#' dir.create(folder)
#' write_to_hdf5(models, folder = folder)
#' list.files(folder)
#'
#' @export
write_to_hdf5.modellist <- function(object, folder, ...){
  
  if (dir.exists(folder)) {

    paths <- vector("list", length(object))

    for (i in seq_along(object)) {
      
      # A model of any class -- a dynamic factor model of dfmtools beside the
      # VARs and VECs of this package, say -- is written on its own by its own
      # method; only a 'modellist' or an 'expandingwindow' is a collection to
      # be gone through. Testing for the two classes of this package instead
      # took every other model for a collection and tried to write its 'data'
      # and 'model' elements as models.
      if (!.is_model_container(object[[i]])) {
        
        # Get model type from the first object in the list
        model_type <- object[[i]][["model"]][["algorithm"]]
        
        p <- sprintf(paste0("%0", 2, "d"), object[[i]][["model"]][["p"]])
        model_details <- paste0("p=", p)
        # Only where the model has a lag order of its exogenous variables. A
        # factor model's 'm' counts its observed series and it has no 's'.
        if (!is.null(object[[i]][["model"]][["s"]]) &&
            isTRUE(object[[i]][["model"]][["m"]] > 0)) {
          s <- sprintf(paste0("%0", 2, "d"), object[[i]][["model"]][["s"]])
          model_details <- paste0(model_details, "-s=", s)
        }
        model_details <- paste0(model_details, "-n=", object[[i]][["model"]][["n"]])
        if (!is.null(object[[i]][["model"]][["varsel"]])) {
          varsel_type <- paste0("-varsel=", object[[i]][["model"]][["varsel"]])
        } else {
          varsel_type <- NULL
        }
        
        # Get list of files in the folder
        file_names <- list.files(folder, recursive = FALSE, full.names = FALSE)
        file_names <- file_names[grepl(".h5", file_names)]
        
        iter <- 0
        cond <- TRUE
        while(cond) {
          iter <- iter + 1
          candidate_name <- paste0(model_type,
                                   varsel_type,
                                   "-", model_details,
                                   "-", sprintf(paste0("%0", 3, "d"), iter), ".h5")
          cond <- candidate_name %in% file_names
        }
        file_name <- file.path(folder, candidate_name)
        
        paths[[i]] <- write_to_hdf5(.with_collection_index(object[[i]], i),
                                    filename = file_name, ...)

      } else {
        # A collection of its own, such as the windows of one specification.
        # Each of its models carries the position of the collection in this
        # list, which is what read_models_from_folder() restores the order
        # from: the file and directory names above sort by algorithm instead.
        member <- object[[i]]
        for (j in seq_along(member)) {
          member[[j]] <- .with_collection_index(member[[j]], i)
        }
        paths[[i]] <- write_to_hdf5(member, folder = folder, ...)
      }
    }

    return(invisible(unlist(paths)))

  } else {
    stop("Specified 'folder' does not exist.")
  }
  
}

# A model with its position in the list being written. Stored like
# 'rclass_collection', as an element of 'model' that the per-model writer turns
# into an attribute of the /model group, and removed again by
# read_models_from_folder().
.with_collection_index <- function(model, i) {
  model[["model"]][["rindex_collection"]] <- as.integer(i)
  model
}
