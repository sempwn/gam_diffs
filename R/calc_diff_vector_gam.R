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
#' @return list
#' @export
calc_diff_vector_gam <- function(m, newdata, ci = 0.95, delta = TRUE) {
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
      ci = 0.95, delta = TRUE
    )
    diffs[i - 1] <- res$m
    lc[i - 1] <- res$lc
    uc[i - 1] <- res$uc
  }

  return(list(m = diffs, lc = lc, uc = uc))
}
