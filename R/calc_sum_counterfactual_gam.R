#' Calculate the variance between a baseline sum of predictions
#' against a counterfactual sum of predictions in a [gam]
#' based on modified code from gam predict function help
#' @param m model object
#' @param baseline_data data.frame of baseline predictors
#' @param counter_data data.frame of counterfactual predictors. `counter_data`
#' cannot be `NULL`
#' @param ci range of confidence interval
#' @return list
#' @export
calc_sum_counterfactual_gam <- function(m, baseline_data,
                                        counter_data = NULL,
                                        ci = 0.95, delta = TRUE) {


  n_baseline <- nrow(baseline_data)
  total_rows <- n_baseline
  newdata <- baseline_data

  if(is.null(counter_data)){


    U <- rep(1,total_rows)
    # compare to 0
    U <- c(U,rep(0,total_rows))


  }else{
    n_counter <- nrow(counter_data)
    total_rows <- total_rows + n_counter
    U <- c(rep(1,n_baseline),rep(0,n_counter))
    U <- c(U,rep(0,n_baseline),rep(-1,n_counter))

    newdata <- dplyr::bind_rows(newdata, counter_data)
  }

  # Create the difference matrix
  U <- matrix(U, nrow = total_rows, ncol = 2)

  res <- calc_generic_vector_gam(m, newdata, U = U,
                                 ci = 0.95, delta = TRUE,
                                 use_relative_diff = FALSE)


  return(res)
}
