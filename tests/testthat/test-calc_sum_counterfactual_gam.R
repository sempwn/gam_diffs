test_that("sum counterfactual gam returns m lc and uc", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x),data = res, family=poisson)
  baseline_data <- dplyr::tibble(x=0:20)
  counter_data <- dplyr::tibble(21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95, delta = TRUE)

  expect_setequal(names(test_diffs),c("m","lc","uc"))
})
