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
#' @param delta Use delta method for log link function
#' @param use_relative_diff Use delta approximation to relative difference
#' @return list
#' @noRd
calc_generic_vector_gam <- function(m, newdata, U = NULL,
                                    ci = 0.95, delta = TRUE,
                                    use_relative_diff = FALSE) {
  npreds <- nrow(newdata)

  if (npreds < 2) {
    stop("newdata needs two rows or more.")
  }

  # check that U has right dimensions
  if (!(nrow(U) == npreds)) {
    stop(glue::glue("U must have rows of length {npreds}"))
  }

  if (ncol(U) != 2) {
    stop("U must have 2 columns: one for baseline and one for comparison.")
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
  grad_g <- gradient_link_function_vector(m,preds,Xp)
  points_vcov <- grad_g %*% stats::vcov(m) %*% base::t(grad_g)

  preds <- exp(preds)


  # Calculate the variance-covariance matrix
  mat_varcov <- base::t(U) %*% points_vcov %*% U

  # mean vector of summed means
  mean_vec <- base::t(U) %*% preds

  if (use_relative_diff) {
    calced_diff <- var_relative_diff(mean_vec, mat_varcov)
  } else {
    calced_diff <- var_absolute_diff(mean_vec, mat_varcov)
  }

  pred_var <- calced_diff$pred_var
  pred_diffs <- calced_diff$pred_diffs


  se <- sqrt(pred_var)


  lc <- pred_diffs - z * se
  uc <- pred_diffs + z * se

  return(list(m = pred_diffs, lc = lc, uc = uc))
}

#' Variance of the absolute difference of X - Y
#' @param vec_mean vector of means x and y
#' @param mat_varcov covariance matrix for x and y
#' @return numeric
#' @noRd
var_absolute_diff <- function(vec_mean, mat_varcov) {
  var_x <- mat_varcov[1, 1]
  var_y <- mat_varcov[2, 2]
  cov_xy <- mat_varcov[1, 2]

  mean_x <- vec_mean[1]
  mean_y <- vec_mean[2]

  pred_var <- var_x + var_y - 2 * cov_xy
  pred_diffs <- (mean_x - mean_y)

  result <- list(pred_diffs = pred_diffs, pred_var = pred_var)

  return(result)
}


#' Relative difference approximation of x/y variance using the delta method
#' @param vec_mean vector of means x and y
#' @param mat_varcov covariance matrix for x and y
#' @return numeric
#' @noRd
var_relative_diff <- function(vec_mean, mat_varcov) {
  var_x <- mat_varcov[1, 1]
  var_y <- mat_varcov[2, 2]
  cov_xy <- mat_varcov[1, 2]

  mean_x <- vec_mean[1]
  mean_y <- vec_mean[2]

  pred_var <- mean_y^2 * var_x + mean_x^2 * var_y - mean_x * mean_y * cov_xy
  pred_var <- pred_var / mean_y^4
  pred_diffs <- (mean_x - mean_y) / mean_y

  result <- list(pred_diffs = pred_diffs, pred_var = pred_var)

  return(result)
}


#' gradient for log link function
#' @param m model object
#' @param preds vector of predictors
#' @param Xp a matrix giving the fitted values of each term in the model
#'   formula on the linear predictor scale
#' @return numeric vector
#' @noRd
gradient_link_function_vector <- function(m,preds,Xp){
  family_name <- stats::family(m)$family
  if(family_name == "poisson"){
    grad_g <- array(exp(preds), c(length(preds), length(stats::coef(m)))) * Xp
  }else if(family_name == "binomial"){
    exp_preds <- array(exp(preds), c(length(preds), length(stats::coef(m))))
    grad_g <-  exp_preds / (1 + exp_preds)^2 * Xp
  }else if(family_name == "exp"){
    stop("Not yet implemented!")
    grad_g <- NULL
  } else{
    stop(glue::glue("Family {family_name} not implemented for delta method!"))
  }
  return(grad_g)
}
