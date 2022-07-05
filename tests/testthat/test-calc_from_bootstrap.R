test_that("Check does not accept wrong method", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  expect_error(
    calc_sum_counterfactual_gam(m, baseline_data,
                                              counter_data = counter_data,
                                              ci = 0.95,
                                              method = "boostrap"

    )
  )
})

test_that("sum counterfactual gam returns m lc uc and total_n from bootstrap", {
  set.seed(42)
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
                                            counter_data = counter_data,
                                            ci = 0.95,
                                            method = "bootstrap",
                                            nrep = 10

  )

  expect_setequal(names(test_diffs), c("m", "lc", "uc", "total_n"))
})


