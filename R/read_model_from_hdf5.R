#' Import Models from HDF5 Files
#'
#' Imports model information and posterior draws from an HDF5 file.
#'
#' @param filename Path to an HDF5 file containing model data.
#' @param group the group the model's tree hangs under inside its file.
#' Defaults to \code{""}, the root of the file, which is where a file holding a
#' single model puts it. See 'Details'.
#' @param draws the draws to read, as their positions in the chain. Defaults to
#' \code{NULL}, every draw. An integer vector reads those draws of every block
#' of the posterior, and \code{integer(0)} reads none of them, which gives the
#' model, its data and its priors without the draws. See 'Details'.
#'
#' @details
#'
#' With a \code{group} every path the reader looks for is read under it
#' instead of at the root, so one file can hold several models side by side.
#' \code{\link{read_model_groups}} reports which groups of a file hold one.
#'
#' The spelling is the one the BayesTS command line uses for its \code{--group}
#' flag: a leading slash and no trailing slash, with \code{""} for the root.
#'
#' \code{draws} reads part of a chain. Every block of a posterior holds one row
#' per draw, and only the rows asked for are read from the file, so a caller
#' that works through a long chain in pieces -- solving a global model draw by
#' draw, for instance -- never holds more of it than the piece it is working
#' on. \code{integer(0)} reads a model without its draws, which is what a step
#' needs that only looks at what a model is.
#'
#' A partial read cannot describe the chain it came from, so the blocks it
#' returns are labelled as a chain of their own, from one to the number of draws
#' read. A full read keeps the labels the file carries.
#'
#' \strong{Which object a file becomes.} This package defines no model class.
#' The class is the one recorded in the file's \code{/model/rclass}, and the
#' package that defines it the one in \code{/model/rpackage}. A file without
#' them -- as the BayesTS command line writes one -- is placed by the prefix of
#' its algorithm:
#' \tabular{lll}{
#'   Prefix \tab Package \tab Class\cr
#'   \code{Var} \tab \pkg{bvartools} \tab 'bvarmodel'\cr
#'   \code{Vec} \tab \pkg{bvartools} \tab 'bvecmodel'\cr
#'   \code{Dfm} \tab \pkg{dfmtools} \tab 'dfmodel'\cr
#'   \code{Favar} \tab \pkg{dfmtools} \tab 'favarmodel'\cr
#'   \code{Uc} \tab \pkg{ucmtools} \tab 'ucmodel'
#' }
#' That package's namespace is loaded, so that its methods are registered
#' whether or not it is attached, and the file is handed to its
#' \code{\link{read_hdf5_model}} method -- or, where it has none, read as a tree and
#' handed to its \code{\link{from_bayests_tree}} method.
#'
#' @return An object of the class recorded in the file, as the package that
#' defines it builds it, with the elements that file holds: \code{model},
#' \code{data}, \code{priors}, \code{initial} and, where the model has been
#' estimated, \code{posterior}. The draws of a sampler are
#' \code{\link[coda]{mcmc}} objects.
#'
#' @seealso \code{\link{read_hdf5_model}}, \code{\link{from_bayests_tree}},
#' \code{\link{read_bayests_tree}}, \code{\link{write_to_hdf5}}.
#'
#' @export
read_model_from_hdf5 <- function(filename, group = "", draws = NULL) {

  group <- .normalize_hdf5_group(group)
  draws <- .check_read_draws(draws)

  h5_file <- hdf5r::h5file(filename, mode = "r")
  on.exit(if (h5_file$is_valid) h5_file$close_all(), add = TRUE)

  # Every path below is named against this rather than against the file, which
  # is all a group amounts to on the way in.
  h5_root <- .hdf5_model_root(h5_file, group)

  if (!"model" %in% names(h5_root)) {
    stop("File ", filename, " does not contain model specification",
         if (group != "") paste0(" in group ", group), ".")
  }

  model_class <- .stored_model_class(h5_root)
  read_hdf5_model(structure(list(), class = model_class), h5_root = h5_root,
            draws = draws)
}


#' Read a Model of a Given Class from an Open File
#'
#' The step of \code{\link{read_model_from_hdf5}} that reads the groups of a
#' model file into the object of the package that defines the model's class.
#'
#' @param x an empty object of the class to read, \code{structure(list(),
#' class = ...)}. It carries nothing but the class it dispatches on.
#' @param h5_root the 'H5Group' or 'H5File' of the open file that the model's
#' tree hangs under, as \pkg{hdf5r} returns it. The caller opens and closes the
#' file.
#' @param draws the positions of the draws to read, or \code{NULL} for every
#' draw, as checked by \code{\link{read_model_from_hdf5}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @details
#'
#' A package whose models are written with \code{\link{write_bayests_tree}}
#' needs no method here: the default reads the tree, gives it the class of
#' \code{x} and hands it to \code{\link{from_bayests_tree}}, where the package
#' undoes the translation its writer made. A method is for a package that
#' reads its files group by group instead -- one that has done so since before
#' the tree existed, or that has to read a large posterior selectively.
#'
#' @return An object of the class of \code{x}.
#'
#' @export
read_hdf5_model <- function(x, h5_root, draws = NULL, ...) {
  UseMethod("read_hdf5_model")
}

#' @rdname read_hdf5_model
#' @export
read_hdf5_model.default <- function(x, h5_root, draws = NULL, ...) {
  tree <- .hdf5_read_tree(h5_root, draws, "")
  class(tree) <- class(x)
  from_bayests_tree(tree)
}


# The draws to read, checked once so that a bad request fails before the file is
# opened rather than on the first block that is read.
.check_read_draws <- function(draws) {

  if (is.null(draws)) {
    return(NULL)
  }
  if (!is.numeric(draws) || anyNA(draws) || any(draws %% 1 != 0)) {
    stop("Argument 'draws' must be whole numbers, the positions of the draws to read.")
  }
  draws <- as.integer(draws)
  if (length(draws) > 0 && min(draws) < 1) {
    stop("Argument 'draws' must be positions of draws, so at least one.")
  }

  draws
}

# One block of a posterior, as a chain where it is one and as a matrix where it
# is not.
#
# Everything a sampler writes carries coda's mcpar -- the start, the end and the
# thinning interval of the chain it came from -- and is read back as an 'mcmc'
# object. The discounted models of bvartools write no such attributes, and
# correctly: their posterior is one column per period of a closed form rather
# than one row per draw of a chain, so there is no start, no end and nothing
# thinned. A block without them is read as the plain matrix it is. Labelling it
# as a chain would make a row of it look like a draw, and the rows are periods.
.read_posterior_block <- function(dataset, draws) {

  block <- .read_draw_rows(dataset, draws)

  if (!"start" %in% hdf5r::h5attr_names(dataset)) {
    return(block)
  }
  if (!is.null(draws)) {
    return(coda::mcmc(block))
  }

  coda::mcmc(block,
             start = hdf5r::h5attr(dataset, "start"),
             end = hdf5r::h5attr(dataset, "end"),
             thin = hdf5r::h5attr(dataset, "thin"))
}

# One block of a posterior, as a matrix of the draws asked for. Rows are read
# from the file rather than read and then subset, which is what keeps a partial
# read small.
.read_draw_rows <- function(dataset, draws) {

  dims <- dataset$dims
  n_draws <- dims[1]
  n_columns <- if (length(dims) > 1) dims[2] else 1L

  if (is.null(draws)) {
    # Shaped from what the file says rather than from what hdf5r hands back.
    # A dataset with one row comes back as a plain vector, dimensions dropped,
    # and as.matrix() then makes a column of it -- so a block of one draw over
    # many columns was read transposed, as many draws of one column. Nothing a
    # sampler writes has one draw, which is why this went unseen until the
    # discounted models, whose /posterior/loglik is exactly that: one row, the
    # exact pointwise log marginal likelihood over the sample.
    return(matrix(hdf5r::readDataSet(dataset), nrow = n_draws, ncol = n_columns))
  }

  if (length(draws) == 0) {
    return(matrix(numeric(0), nrow = 0, ncol = n_columns))
  }
  if (max(draws) > n_draws) {
    stop("Draw ", max(draws), " was asked for, and the chain holds ", n_draws, ".")
  }

  if (length(dims) > 1) {
    matrix(dataset[draws, ], nrow = length(draws), ncol = n_columns)
  } else {
    matrix(dataset[draws], nrow = length(draws), ncol = 1L)
  }
}


# The class a model file is read as, with the namespace of the package that
# defines it loaded.
#
# The class is the one recorded in /model/rclass, and the package the one in
# /model/rpackage, which a model package records when it writes the file. A
# file without them, as the BayesTS command line writes one, is placed by its
# algorithm in .algorithm_packages. The namespace is loaded so that a model is
# read the same way whether or not its package is attached -- by a worker of
# map_models(), say.
#
# The package is looked up rather than named in a call, so that this package
# depends on none of the packages whose models it can read.
.stored_model_class <- function(h5_root) {

  attrs <- hdf5r::h5attributes(h5_root[["model"]])
  algorithm <- attrs[["algorithm"]]
  rclass <- attrs[["rclass"]]

  known <- .algorithm_package(algorithm)
  package <- attrs[["rpackage"]]
  if (is.null(package)) {
    package <- known[["package"]]
  }
  if (is.null(rclass)) {
    rclass <- known[["class"]]
  }
  if (is.null(rclass)) {
    stop("The file holds a model of algorithm '",
         if (is.null(algorithm)) "" else algorithm, "', which records no class ",
         "in /model/rclass and belongs to no package this one knows of. Read ",
         "the raw tree with read_bayests_tree().")
  }

  if (!is.null(package) && !requireNamespace(package, quietly = TRUE)) {
    stop("The file holds a ", if (is.null(algorithm)) "" else paste0(algorithm, " "),
         "model of package ", package, ". Install ", package, " to read it, ",
         "or read the raw tree with read_bayests_tree().")
  }

  rclass
}

# The package and class of an algorithm of the BayesTS command line, by the
# longest prefix of its name in .algorithm_packages, or NULL.
.algorithm_package <- function(algorithm) {

  if (is.null(algorithm)) {
    return(NULL)
  }
  known <- NULL
  matched <- 0L
  for (prefix in names(.algorithm_packages)) {
    if (startsWith(algorithm, prefix) && nchar(prefix) > matched) {
      known <- .algorithm_packages[[prefix]]
      matched <- nchar(prefix)
    }
  }

  known
}

# The models the BayesTS command line writes, by the prefix of their algorithm:
# the package that reads them and the class they are read as.
.algorithm_packages <- list(
  "Var" = list("package" = "bvartools", "class" = c("bvarmodel", "list")),
  "Vec" = list("package" = "bvartools", "class" = c("bvecmodel", "list")),
  "Dfm" = list("package" = "dfmtools", "class" = c("dfmodel", "list")),
  "Favar" = list("package" = "dfmtools", "class" = c("favarmodel", "list")),
  "Uc" = list("package" = "ucmtools", "class" = c("ucmodel", "list"))
)
