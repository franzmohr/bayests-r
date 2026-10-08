# Reduce a variance decomposition to at most `max_groups` columns, so that the
# legend of the plot stays readable for a model with many variables.
#
# The `max_groups - 1` columns with the largest contribution across the whole
# horizon are kept in the order of the endogenous variables and the remaining
# ones are added up in a column "Other", which occupies the last of the slots.
# Row sums are left untouched, so a decomposition that added up to one before
# still does afterwards.
.limit_fevd_groups <- function(x, max_groups) {

  keep <- sort(order(colSums(x), decreasing = TRUE)[seq_len(max_groups - 1)])
  rest <- setdiff(seq_len(ncol(x)), keep)

  label <- "Other"
  while (label %in% colnames(x)[keep]) {
    label <- paste0(label, "_")
  }

  result <- cbind(x[, keep, drop = FALSE], rowSums(x[, rest, drop = FALSE]))
  colnames(result) <- c(colnames(x)[keep], label)

  return(result)
}

# Validate the `max_groups` argument of fevd() and plot.bvarfevd(), which offer
# it with the same meaning and should refuse the same input. Returns NULL, which
# stands for "show every variable", or the value as an integer.
.check_max_groups <- function(max_groups) {

  if (is.null(max_groups)) {
    return(NULL)
  }

  if (length(max_groups) != 1 || !is.numeric(max_groups) || is.na(max_groups) || max_groups < 1) {
    stop("Argument 'max_groups' must be a single positive integer.")
  }

  return(as.integer(max_groups))
}
