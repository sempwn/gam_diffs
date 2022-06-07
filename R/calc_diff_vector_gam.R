#' Calculate the variance between a baseline prediction
#' and subsequent predictions in a gam model
#' based on modified code from gam predict function help
#' @param m model object
#' @param newdata data.frame nrow >= 2 defining points to compare to the first
#' row in the data frame
#' @param ci range of confidence interval
#' @param delta boolean determines whether to apply the delta approximation
#' to a log link function
#' @return list
#' @export
calc_diff_vector_gam <- function(m, newdata, ci = 0.95, delta = TRUE) {
  npreds <- nrow(newdata)

  if (npreds < 2) {
    stop("newdata needs two rows or more.")
  }

  # Create the difference matrix
  U <- diag(npreds)
  U[1, ] <- -1

  res <- calc_generic_vector_gam(m, newdata, U = U,
                                 ci = 0.95, delta = TRUE)


  diffs <- res$m
  lc <- res$lc
  uc <- res$uc

  # only include the differences
  diffs <- diffs[-1, ]
  lc <- lc[-1, ]
  uc <- uc[-1, ]

  return(list(m = diffs, lc = lc, uc = uc))
}
