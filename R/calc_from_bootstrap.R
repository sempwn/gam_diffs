#' Calculate difference of vector of model inputs by bootstrapping original
#' data
#' @param m model object
#' @param newdata data
#' @param U difference matrix. defines the vector of differences being computed
#' @param ci range of confidence interval
#' @param nrep number of samples of posterior to compute ci
#' @returns
#' An `list` object. The output has the following elements:
#'
#' * m - mean
#' * lc - lower confidence interval
#' * uc - upper confidence interval
#' * total_n - total number of samples in bootstrap
#' @noRd
#' @importFrom stats as.formula
calc_generic_vector_from_bootstrap <- function(m,
                                     newdata,
                                     U = NULL,
                                     ci = 0.95,
                                     use_relative_diff = FALSE,
                                     nrep = 100) {

  # extract original data to fit model
  model_data <- m$model

  npreds <- nrow(newdata)

  input_check(npreds,U)

  # flat vector version of 2 x npreds
  V <- U[, 1] - U[, 2]
  V <- matrix(V, ncol = npreds)

  # baseline U vector used in relative change calculation
  V_baseline <- U[, 2]
  V_baseline <- matrix(V_baseline, ncol = npreds)

  opt <- rep(NA, nrep)


  # progress bar
  pb <- progress::progress_bar$new(
    format = "  bootstrapping [:bar] :percent eta: :eta",
    total = nrep, clear = FALSE, width = 60
  )

  for (i in 1:nrep) {
    int_sample <- sample(1:nrow(model_data), nrow(model_data), replace = TRUE)
    sample_data <- model_data[int_sample, ]

    preds <- tryCatch(
      {
        m_sample <- mgcv::gam(as.formula(m$formula),
                        data = sample_data,
                        family = m$family
        )

        predict(m_sample, newdata = newdata, type = "response")
      },
      error = function(cond) {
        return(rep(NA, npreds))
      },
      warning = function(cond) {
        return(rep(NA, npreds))
      }
    )



    opt[i] <- V %*% preds

    if(use_relative_diff){
      baseline_opt <- V_baseline %*% preds
      opt[i] <- opt[i]/baseline_opt
    }


    pb$tick()
  }

  total_n <- sum(!is.na(opt))

  diffs <- mean(opt, na.rm = TRUE)
  lc <- stats::quantile(opt, probs = (1 - ci) / 2, na.rm = TRUE)
  uc <- stats::quantile(opt, probs = 1 - (1 - ci) / 2, na.rm = TRUE)

  return(list(m = diffs, lc = lc, uc = uc, total_n = total_n))
}



#' Calculate difference of vector of model inputs by bootstrapping original
#' data
#' @param m model object
#' @param newdata data
#' @param U difference matrix. defines the vector of differences being computed
#' @param ci range of confidence interval
#' @param nrep number of samples of posterior to compute ci
#' @returns
#' An `list` object. The output has the following elements:
#'
#' * m - mean
#' * lc - lower confidence interval
#' * uc - upper confidence interval
#' * total_n - total number of samples in bootstrap
#' @noRd
#' @importFrom stats as.formula delete.response reformulate resid terms
calc_generic_vector_from_efron_bootstrap <- function(m,
                                               newdata,
                                               U = NULL,
                                               ci = 0.95,
                                               use_relative_diff = FALSE,
                                               nrep = 100) {

  # extract original data to fit model
  model_data <- m$model

  npreds <- nrow(newdata)

  input_check(npreds,U)

  # get predictions
  m_preds <- predict(m)

  # get residuals
  m_resids <- resid(m)#analytic_ed_data$counts - m_preds

  # get number of data points for original data used in fitting
  ndata <- length(m_preds)

  ilink <- family(m)$linkinv
  link <- family(m)$link
  family <- family(m)$family

  # reformulate formula to have y as response
  formula_terms <- terms(m$formula)
  term_labels <- attr(delete.response(formula_terms),"term.labels")
  y_formula <- reformulate(term_labels,response = "y")

  # flat vector version of 2 x npreds
  V <- U[, 1] - U[, 2]
  V <- matrix(V, ncol = npreds)

  # baseline U vector used in relative change calculation
  V_baseline <- U[, 2]
  V_baseline <- matrix(V_baseline, ncol = npreds)

  opt <- rep(NA, nrep)


  # progress bar
  pb <- progress::progress_bar$new(
    format = "  bootstrapping [:bar] :percent eta: :eta",
    total = nrep, clear = FALSE, width = 60
  )

  for (i in 1:nrep) {
    sample_y <- m_preds + sample(m_resids, ndata, replace = TRUE)
    sample_data <- model_data
    sample_data[, "y"] <- as.numeric(ilink(sample_y))

    if(link == "log"){
      # need to ensure discrete
      sample_data[, "y"] <- floor(sample_data[, "y"])
    }else{
      # no need to update
      sample_data[, "y"] <- sample_data[, "y"]
    }

    preds <- tryCatch(
      {
        m_sample <- mgcv::gam(as.formula(y_formula),
                              data = sample_data,
                              family = m$family
        )

        predict(m_sample, newdata = newdata, type = "response")
      },
      error = function(cond) {
        return(rep(NA, npreds))
      },
      warning = function(cond) {
        return(rep(NA, npreds))
      }
    )



    opt[i] <- V %*% preds

    if(use_relative_diff){
      baseline_opt <- V_baseline %*% preds
      opt[i] <- opt[i]/baseline_opt
    }


    pb$tick()
  }

  total_n <- sum(!is.na(opt))

  diffs <- mean(opt, na.rm = TRUE)
  lc <- stats::quantile(opt, probs = (1 - ci) / 2, na.rm = TRUE)
  uc <- stats::quantile(opt, probs = 1 - (1 - ci) / 2, na.rm = TRUE)

  return(list(m = diffs, lc = lc, uc = uc, total_n = total_n))
}





