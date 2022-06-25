#' calculate difference of set of vectors by bootstrapping
#' @param m model object
#' @param newdata data.frame nrow >= 2 defining points to compare to the first
#' row in the data frame
#' @param U difference matrix. defines the vector of differences being computed
#' @param ci range of confidence interval
#' @param nrep number of samples of posterior to compute ci
calc_generic_vector_from_post <- function(m, newdata,
                                          U = NULL,
                                          ci = 0.95,
                                          use_relative_diff = FALSE,
                                          nrep = 1000) {
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

  # flat vector version of 2 x npreds
  V <- U[, 1] - U[, 2]
  V <- matrix(V, ncol = npreds)

  # baseline U vector used in relative change calculation
  V_baseline <- U[, 2]
  V_baseline <- matrix(V_baseline, ncol = npreds)

  # linear predictor matrix
  Xp <- predict(m, newdata = newdata, type = "lpmatrix")

  # get mean covariance for multivariate normal of parameter posterior
  beta <- coef(m)
  Vb <- vcov(m)

  ## simulate nrep coef vectors from posterior
  ## dimensions: nrep x npreds
  mrand <- MASS::mvrnorm(nrep, beta, Vb)

  ilink <- family(m)$linkinv
  opt <- V %*% ilink(Xp %*% t(mrand))

  # TODO check opt above and add in calc of difference and optional
  # relative diff below.
  if(use_relative_diff){
    baseline_opt <- V_baseline %*% ilink(Xp %*% t(mrand))
    opt <- opt/baseline_opt
  }

  diffs <- mean(opt)
  lc <- stats::quantile(opt, probs = (1 - ci) / 2)
  uc <- stats::quantile(opt, probs = 1 - (1 - ci) / 2)


  return(list(m = diffs, lc = lc, uc = uc))
}

#' calculate difference by simulating from the posterior
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the variance between a baseline prediction
#' and subsequent prediction in a GAM model using samples from the
#' posterior.
#'
#' @param m model object
#' @param newdata data.frame nrow = 2 defining two points to compare
#' @param ci range of confidence interval
#' @param use_relative_diff provide estimates as a relative difference, otherwise
#' presented as an absolute difference
#' @param nrep number of samples of posterior to compute ci
calc_diff_from_post <- function(m, newdata, ci = 0.95,
                                use_relative_diff = FALSE,
                                nrep = 1000) {
  if (nrow(newdata) != 2) {
    stop("newdata needs two rows.")
  }

  # linear predictor matrix
  Xp <- predict(m, newdata = newdata, type = "lpmatrix")

  # get mean covariance for multivariate normal of parameter posterior
  beta <- coef(m)
  Vb <- vcov(m)

  ## simulate nrep coef vectors from posterior
  mrand <- MASS::mvrnorm(nrep, beta, Vb)

  opt <- rep(NA, nrep)
  ilink <- family(m)$linkinv
  for (i in seq_len(nrep)) {
    pred <- ilink(Xp %*% mrand[i, ])
    if (use_relative_diff) {
      opt[i] <- (pred[1] - pred[2]) / pred[2]
    } else {
      opt[i] <- pred[1] - pred[2]
    }
  }

  diffs <- mean(opt)
  lc <- quantile(opt, (1 - ci) / 2)
  uc <- quantile(opt, 1 - (1 - ci) / 2)

  return(list(m = diffs, lc = lc, uc = uc))
}
