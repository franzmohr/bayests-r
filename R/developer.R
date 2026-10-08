#' Functions for Model Packages
#'
#' Internal functions of this package that the packages defining model classes
#' call from their methods. They are exported for those packages and are not
#' meant to be called in an analysis; their arguments and results may change
#' with the needs of the model packages.
#'
#' @details
#' \describe{
#'   \item{Seeds}{\code{.check_seed(seed)} checks and converts a seed,
#'   \code{.draw_model_seed()} draws one from R's generator and
#'   \code{.offset_seed(seed, n)} gives the seed of the \code{n}-th model of a
#'   list numbered from \code{seed}.}
#'   \item{Draws}{\code{.draws_matrix(draws)} is an 'mcmc' object as a plain
#'   matrix with its dimnames.}
#'   \item{Writing model files}{\code{.hdf5_write_model(filename, group,
#'   write)} opens a file for a model and undoes a write that fails;
#'   \code{.hdf5_group}, \code{.hdf5_write} and \code{.hdf5_write_attr} write
#'   groups, datasets and attributes below it; \code{.hdf5_draws_attrs} and
#'   \code{.hdf5_series_attrs} are the attributes that let draws and time
#'   series be read back as what they were.}
#'   \item{Reading model files}{\code{.hdf5_read_matrix},
#'   \code{.hdf5_read_value}, \code{.hdf5_restore_class} and
#'   \code{.read_posterior_block} read datasets as the writers above wrote them,
#'   for a \code{\link{read_hdf5_model}} method.}
#'   \item{Plots}{\code{draw_error_bars} draws the error bars of the
#'   forecast error plots, and \code{forecast_plot_series} turns a forecast of
#'   class 'bvarprd' into the series its plot draws.}
#'   \item{Variance decompositions}{\code{.check_max_groups} and
#'   \code{.limit_fevd_groups} check and apply the limit on the number of
#'   shock groups a decomposition shows.}
#'   \item{Model comparison}{\code{.log_mean_exp}, \code{.nse_log_mean_exp},
#'   \code{.lpl_entry}, \code{.loo_data_frame} and \code{.waic_data_frame} are
#'   the pieces \code{\link{selection_criteria}} methods assemble their results
#'   from.}
#' }
#'
#' @name bayests-developer
#' @aliases draw_error_bars forecast_plot_series .check_max_groups .limit_fevd_groups .check_seed .draw_model_seed .offset_seed .draws_matrix .hdf5_write_model .hdf5_group .hdf5_write .hdf5_write_attr .hdf5_draws_attrs .hdf5_series_attrs .hdf5_read_matrix .hdf5_read_value .hdf5_restore_class .read_posterior_block .log_mean_exp .nse_log_mean_exp .lpl_entry .loo_data_frame .waic_data_frame
#' @keywords internal
#' @rawNamespace export(.check_seed, .draw_model_seed, .offset_seed, .draws_matrix)
#' @rawNamespace export(.hdf5_write_model, .hdf5_group, .hdf5_write, .hdf5_write_attr)
#' @rawNamespace export(.hdf5_draws_attrs, .hdf5_series_attrs)
#' @rawNamespace export(.hdf5_read_matrix, .hdf5_read_value, .hdf5_restore_class)
#' @rawNamespace export(.read_posterior_block)
#' @rawNamespace export(.log_mean_exp, .nse_log_mean_exp, .lpl_entry)
#' @rawNamespace export(.loo_data_frame, .waic_data_frame, draw_error_bars)
#' @rawNamespace export(forecast_plot_series, .check_max_groups, .limit_fevd_groups)
NULL
