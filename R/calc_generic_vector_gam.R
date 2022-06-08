#' Calculate the variance between a baseline prediction
#' and subsequent predictions in a gam model
#' based on modified code from gam predict function help
#' @description
#' `r lifecycle::badge("experimental")`
#' @param m model object
#' @param newdata data.frame nrow >= 2 defining points to compare to the first
#' row in the data frame
#' @param U difference matrix. defines the vector of differences being computed
#' @param ci range of confidence interval
#' @return list
#' @noRd
calc_generic_vector_gam <- function(m, newdata, U = NULL,
                                    ci = 0.95, delta = TRUE) {
  npreds <- nrow(newdata)

  if (npreds < 2) {
    stop("newdata needs two rows or more.")
  }

  # check that U has right dimensions
  if (!(nrow(U) == npreds)) {
    stop(glue::glue("U must have rows of length {npreds}"))
  }

  # calculate quantile for % ci
  p <- 1 - (1 - ci) / 2
  z <- stats::qnorm(p)

  # linear predictor matrix
  Xp <- stats::predict(m, newdata = newdata, type = "lpmatrix")
  # variance covariance of predicted values
  # a function with a generic name like t and no proper handling of
  # namespace, are you kidding me??
  points_vcov <- Xp %*% stats::vcov(m) %*% base::t(Xp)

  # predicted difference
  preds <- Xp %*% stats::coef(m)



  # calculate the variance of an exponential using the delta method
  grad_g <- array(exp(preds), c(length(preds), length(stats::coef(m)))) * Xp
  points_vcov <- grad_g %*% stats::vcov(m) %*% base::t(grad_g)

  preds <- exp(preds)


  # Calculate the variance vector

  pred_var <- diag(base::t(U) %*% points_vcov %*% U)
  se <- sqrt(pred_var)

  diffs <- base::t(U) %*% preds
  lc <- diffs - z * se
  uc <- diffs + z * se

  return(list(m = diffs, lc = lc, uc = uc))
}


#' Relative difference approximation of x/y variance using the delta method
#' @param vec_mean vector of means x and y
#' @param mat_varcov covariance matrix for x and y
#' @return numeric
#' @noRd
relative_diff_delta <- function(vec_mean,mat_varcov){
  var_x <- mat_varcov[1,1]
  var_y <- mat_varcov[2,2]
  cov_xy <- mat_varcov[1,2]

  mean_x <- vec_mean[1]
  mean_y <- vec_mean[2]

  result <- mean_x^2 * var_x + mean_x^2 * var_y + mean_x * mean_y * cov_xy
  result <- result / mean_y^4

  return(result)

}
