#' Expected Size of a Model
#'
#' A generic function that calculates how much memory a model will take once its
#' posterior draws are complete, and so roughly how much disk space it will take
#' when written to a file.
#'
#' @param object an object of a class, for which a method should be called.
#' @param ... arguments passed forward to method.
#'
#' @details The functions that draw or write a model warn before they start when
#' the result would exceed the limit set by \code{options(bayests.size_warning)},
#' in bytes, which is 1e9, one gigabyte, unless set otherwise, and \code{Inf}
#' turns the warnings off. \code{\link{add_posterior_coefficients}} checks the
#' size \code{expected_model_size()} predicts for the draws it is about to simulate,
#' and \code{\link{write_to_hdf5}} checks the size of the object it is about to
#' write. A package that adds a model class gets both warnings by adding a method
#' to this generic.
#'
#' @return The value returned by the method for the class of \code{object},
#' as described on the pages of the methods.
#'
#' @seealso Methods: \code{\link[bvartools]{expected_model_size.bvarmodel}},
#' \code{\link[bvartools:expected_model_size.bvarmodel]{expected_model_size.bvecmodel}},
#' \code{\link{expected_model_size.expandingwindow}},
#' \code{\link{expected_model_size.modellist}}.
#'
#' @export
expected_model_size <- function(object, ...) {
  UseMethod("expected_model_size")
}

#' @export
expected_model_size.default <- function(object, ...) {
  stop("expected_model_size() has no method for an object of class '",
       paste(class(object), collapse = "', '"), "'. Its methods come with the ",
       "packages that define model classes, such as bvartools and dfmtools, ",
       "and cover the lists of them of class 'modellist' and 'expandingwindow'.")
}

#' @export
print.modelsize <- function(x, ...) {

  steps <- unique(x[["step"]])
  totals <- vapply(steps, function(step) sum(x[["bytes"]][x[["step"]] == step]), numeric(1))
  labels <- ifelse(steps == "", "data, priors and initial values", paste0(steps, "()"))

  if (is.null(x[["model"]])) {
    cat("Expected size of the model once its posterior is complete:",
        .format_bytes(sum(x[["bytes"]])), "\n\n")
  } else {
    cat("Expected size of the", length(unique(x[["model"]])),
        "models once their posteriors are complete:", .format_bytes(sum(x[["bytes"]])), "\n\n")
  }
  print(data.frame(" " = format(labels), size = .format_bytes(totals),
                   check.names = FALSE, row.names = NULL),
        row.names = FALSE, right = FALSE)

  if (is.null(x[["model"]])) {
    blocks <- as.data.frame(x)[x[["step"]] != "",c("element", "draws", "columns", "bytes")]
    blocks[["bytes"]] <- .format_bytes(blocks[["bytes"]])
    names(blocks)[4] <- "size"
    cat("\n")
    print(blocks, row.names = FALSE, right = FALSE)
  }

  invisible(x)
}

# A number of bytes as a person reads it, in powers of 1000 as a drive's
# capacity is given.
.format_bytes <- function(bytes) {
  vapply(bytes, function(b) {
    format(structure(b, class = "object_size"), units = "auto", standard = "SI")
  }, character(1))
}

# The sizes of the models in a list, one block of rows per model and a column
# naming it. A list inside the list names its models by both.
.model_list_size <- function(object, ...) {
  labels <- names(object)
  if (is.null(labels)) {
    labels <- as.character(seq_along(object))
  }
  parts <- lapply(seq_along(object), function(i) {
    part <- expected_model_size(object[[i]], ...)
    if (is.null(part[["model"]])) {
      part <- cbind(model = labels[i], as.data.frame(part), stringsAsFactors = FALSE)
    } else {
      part[["model"]] <- paste(labels[i], part[["model"]], sep = "/")
    }
    as.data.frame(part)
  })
  result <- do.call(rbind, parts)
  class(result) <- c("modelsize", "data.frame")
  result
}

# The warnings expected_model_size() exists for.
#
# Each is raised once per call from the outside: the collection methods call
# the same generic for every model in them, which would warn once per model
# about the size of one model rather than once about the size of all of them.
# The flag is only set where the size could be computed, so that a list of a
# class this package has no method for -- a 'gvarmodel' of bgvars without its
# own method -- still has its members checked one by one.
#
# A check never stops the call it guards: a size that cannot be computed is not
# a reason to refuse an estimation that may well fit.
.size_check <- new.env(parent = emptyenv())

.size_limit <- function() {
  limit <- getOption("bayests.size_warning", 1e9)
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit)) {
    return(Inf)
  }
  limit
}

# Called by add_posterior_coefficients() before it dispatches. Returns whether
# the object was checked, which is when the caller holds the flag.
.check_posterior_size <- function(object, chains = NULL) {
  size <- tryCatch(expected_model_size(object, chains = chains), error = function(e) NULL)
  if (is.null(size)) {
    return(FALSE)
  }
  bytes <- sum(size[["bytes"]][size[["step"]] %in% c("", "add_posterior_coefficients")])
  limit <- .size_limit()
  if (bytes > limit) {
    warning("Once its posterior is drawn, this ",
            if (is.null(size[["model"]])) "model" else "list of models",
            " is expected to take about ", .format_bytes(bytes),
            " -- of memory, or of disk space where it is kept in files -- which is more ",
            "than the limit of ", .format_bytes(limit), " in options(bayests.size_warning). ",
            "If this machine cannot spare that, lower 'iterations' or raise 'thin' where the ",
            "model was created; expected_model_size() shows which part of the model takes the space. ",
            "options(bayests.size_warning = Inf) turns this warning off.", call. = FALSE)
  }
  TRUE
}

# Called by write_to_hdf5() before it dispatches. What is written is the object
# as it stands, so its size is measured rather than predicted.
.check_file_size <- function(object) {
  bytes <- as.numeric(utils::object.size(object))
  limit <- .size_limit()
  if (bytes > limit) {
    warning("Writing this object is expected to take up to ", .format_bytes(bytes),
            " of disk space, which is more than the limit of ", .format_bytes(limit),
            " in options(bayests.size_warning). The files are compressed, but the draws ",
            "of a sampler compress poorly, so make sure the drive has that much room. ",
            "options(bayests.size_warning = Inf) turns this warning off.", call. = FALSE)
  }
  invisible(NULL)
}
