#' Estimate of differences
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the variance between a baseline prediction
#' and subsequent predictions in a gam model
#' based on modified code from gam predict function help
#' @param m model object
#' @param newdata data.frame nrow >= 2 defining points to compare to the first
#' row in the data frame
#' @param ci range of confidence interval
#' @param delta Boolean determines whether to apply the delta approximation
#' to a log link function
#' @param use_relative_diff provide estimates as a relative difference, otherwise
#' presented as an absolute difference
#' @return list
#' @export
calc_diff_vector_gam <- function(m, newdata, ci = 0.95, delta = TRUE,
                                 use_relative_diff = FALSE) {
  npreds <- nrow(newdata)

  if (npreds < 2) {
    stop("newdata needs two rows or more.")
  }

  diffs <- rep(NA, npreds - 1)
  lc <- rep(NA, npreds - 1)
  uc <- rep(NA, npreds - 1)

  # counterfactual data
  counter_data <- newdata[1, ]

  # Create the difference matrix
  for (i in 2:npreds) {
    baseline_data <- newdata[i, ]


    res <- calc_sum_counterfactual_gam(m, baseline_data,
      counter_data = counter_data,
      ci = ci, delta = delta,
      use_relative_diff = use_relative_diff
    )
    diffs[i - 1] <- res$m
    lc[i - 1] <- res$lc
    uc[i - 1] <- res$uc
  }

  return(list(m = diffs, lc = lc, uc = uc))
}
