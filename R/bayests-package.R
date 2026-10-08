#' @keywords internal
#'
#' @section What the package is for:
#' The samplers of the 'BayesTS' library reach R through packages of their own:
#' 'bvartools' for vector autoregressive and error correction models,
#' 'dfmtools' for dynamic factor models and factor augmented VARs, and further
#' packages for further model classes. Each of them builds its own model
#' objects. What they have in common lives here, so that a model of one package
#' is set up, estimated, stored and compared in the same way as a model of
#' another:
#' \itemize{
#'   \item the generic functions of the workflow, such as
#'   \code{\link{add_priors}}, \code{\link{add_initial_values}},
#'   \code{\link{add_posterior_coefficients}}, \code{\link{add_seed}},
#'   \code{\link{write_to_hdf5}} and \code{\link{selection_criteria}}, whose
#'   methods the model packages define;
#'   \item model files: \code{\link{read_model_from_hdf5}},
#'   \code{\link{read_bayests_tree}}, \code{\link{write_bayests_tree}},
#'   \code{\link{read_model_groups}}, \code{\link{read_model_file}} and
#'   \code{\link{read_model_folder}};
#'   \item lists of models, of class 'modellist' and 'expandingwindow', whose
#'   methods apply a step of the workflow to every model they hold, in parallel
#'   where asked;
#'   \item running models through the 'BayesTS' command line,
#'   \code{\link{bayests_files}} and \code{\link{bayests_posterior}};
#'   \item model comparison: \code{\link{selection_criteria}},
#'   \code{\link{choose_best_model}} and \code{\link{chain_diagnostics}}.
#' }
#'
#' The package estimates no model of its own. A file of a model class is read
#' into the object of the package that defines it, which
#' \code{\link{read_model_from_hdf5}} loads for the purpose.
#'
#' @importFrom coda thin
#' @importFrom stats window
"_PACKAGE"
