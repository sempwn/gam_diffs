test_that("sum counterfactual gam returns m lc and uc from posterior", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95, delta = TRUE,
                                            use_post = TRUE

  )

  expect_setequal(names(test_diffs), c("m", "lc", "uc"))
})

test_that("sum counterfactual gam returns non-zero from posterior", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95, delta = TRUE,
                                            use_post = TRUE

  )
  is_not_zero <- sapply(test_diffs, function(x){x!=0})

  expect_true(all(is_not_zero))
})
