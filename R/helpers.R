#' create random data for purpose of testing
#' @noRd
create_random_data <- function() {
  # for reproducibility
  set.seed(42)
  n <- 100
  x <- seq(1, n, by = 1)
  y <- exp(sin(2 * pi * x / 25) + 0.02 * x)
  y <- stats::rpois(n, lambda = y)

  random_data <- dplyr::tibble(x = x, y = y)
  return(random_data)
}


#' check the input type for [calc_generic_from_post] and [calc_generic_from_bootsrap]
#' functions
input_check <- function(npreds,U){
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
}

#' return vector of method names
get_method_names <- function(){
  c("delta","bootstrap","posterior")
}
