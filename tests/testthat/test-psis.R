test_that("the importance weights are smoothed and normalised", {
  set.seed(20240101)

  # A heavy tailed set of ratios is what the smoothing exists for: the largest
  # weights are pulled in, so the sum stays one and no single draw dominates.
  smoothed <- bayests:::.psis_weights(rt(2000, df = 2), tail_len = 134)
  weights <- exp(smoothed[["log_weights"]])

  expect_equal(sum(weights), 1)
  expect_true(is.finite(smoothed[["pareto_k"]]))
  expect_lt(max(weights), 1)

  # A tail with nothing to fit is left alone rather than smoothed into noise,
  # and is flagged by an infinite shape parameter.
  expect_identical(bayests:::.psis_weights(rep(0, 500), tail_len = 100)[["pareto_k"]], Inf)
  expect_identical(bayests:::.psis_weights(stats::rnorm(20), tail_len = 4)[["pareto_k"]], Inf)
})

test_that("the shape reported is posterior's fit of the tail", {
  set.seed(20240102)
  log_ratios <- rt(2000, df = 3)
  tail_len <- 134

  # The tail as .psis_weights() hands it to the fit: the largest ratios, shifted
  # by the largest and measured from the cutoff below them.
  lw <- sort(log_ratios - max(log_ratios))
  tail <- lw[seq(2000 - tail_len + 1, 2000)]
  cutoff <- lw[2000 - tail_len]

  expected <- posterior::gpdfit(exp(tail) - exp(cutoff), sort_x = FALSE)[["k"]]
  expect_equal(bayests:::.psis_weights(log_ratios, tail_len)[["pareto_k"]], expected)
})

test_that("a tail the Pareto fit cannot handle is flagged, not smoothed", {
  # A quarter or more of the tail at its smallest value leaves the fit's grid
  # without a scale, and posterior::gpdfit() returns NA for it.
  log_ratios <- c(stats::runif(400, -5, 0), rep(1, 60), seq(1.1, 3, length.out = 40))
  smoothed <- bayests:::.psis_weights(log_ratios, tail_len = 100)

  expect_identical(smoothed[["pareto_k"]], Inf)
  expect_equal(sum(exp(smoothed[["log_weights"]])), 1)
})
