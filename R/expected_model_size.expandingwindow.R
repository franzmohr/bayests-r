#' @include expected_model_size.R
#' @rdname expected_model_size
#' @export
expected_model_size.expandingwindow <- function(object, ...) {
  .model_list_size(object, ...)
}
