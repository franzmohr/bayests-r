#' Plotting Forecasts of BVAR Models
#' 
#' A plot function for objects of class 'bvarprd'.
#' 
#' @param x an object of class 'bvarprd', usually, a result of a call to \code{predict.bvarmodel} (\pkg{bvartools}).
#' @param n_pre number of plotted observations that precede the forecasts. If \code{NULL} (default),
#' all available observations will be plotted.
#' @param ci interval used to calculate the credible bands of the forecasts.
#' @param ... further graphical parameters. Arguments \code{main}, \code{ylab},
#' \code{lty} and \code{plot.type} overwrite the defaults of the function.
#' 
#' @return \code{x}, invisibly. The function is called for its side effect, the
#' plot.
#'
#' @examples
#' # Two series with means 1 and -1
#' set.seed(1)
#' y <- ts(cbind(a = rnorm(40, 1), b = rnorm(40, -1)), start = c(2000, 1),
#'         frequency = 4)
#'
#' model <- create_normalmodel(window(y, end = c(2008, 4)), iterations = 200)
#' model <- add_priors(model, mean = 0, prior_variance = 10)
#' model <- add_initial_values(model)
#' model <- add_posterior_coefficients(model)
#' model <- add_forecast_input(model, n_ahead = 8)
#' model <- add_posterior_forecasts(model)
#'
#' # Collect them and plot the last five years with the forecasts
#' pred <- predict(model)
#' plot(pred, n_pre = 20)
#'
#' @export
plot.bvarprd <- function(x, n_pre = NULL, ci = 0.95, ...) {
  
  dots <- list(...)

  series <- forecast_plot_series(x, n_pre = n_pre, ci = ci)

  for (i in names(series)) {
    # Defaults that may be overridden via '...'
    args <- list(plot.type = "single", lty = c(1, 2, 3, 2), main = i, ylab = "")
    args <- args[!(names(args) %in% names(dots))]

    do.call(stats::plot.ts, c(list(series[[i]]), args, dots))
  }

  invisible(x)
}
