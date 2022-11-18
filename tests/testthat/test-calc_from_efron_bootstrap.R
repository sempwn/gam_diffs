test_that("sum counterfactual gam returns m lc uc and toal_n from efron bootstrap", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95,
                                            method = "efron bootstrap",
                                            nrep = 10

  )

  expect_setequal(names(test_diffs), c("m", "lc", "uc", "total_n"))
})

test_that("sum counterfactual gam returns non-zero from efron bootstrap", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95,
                                            method = "efron bootstrap",
                                            nrep = 10

  )
  is_not_zero <- sapply(test_diffs, function(x){x!=0})

  expect_true(all(is_not_zero))
})
