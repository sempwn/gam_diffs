#' create random data for purpose of testing
#' @noRd
create_random_data <- function(){
  # for reproducibility
  set.seed(42)
  n <- 100
  x <- seq(1,n,by=1)
  y <- exp(sin(2 * pi * x/ 25) + 0.02*x)
  y <- stats::rpois(n,lambda=y)

  random_data <- dplyr::tibble(x=x,y=y)
  return(random_data)
}
