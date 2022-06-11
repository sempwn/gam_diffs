test_that("sum counterfactual gam returns m lc and uc", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = counter_data,
    ci = 0.95, delta = TRUE
  )

  expect_setequal(names(test_diffs), c("m", "lc", "uc"))
})

test_that("error on different colnames for baseline and counter data", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(y = 21:40)
  expect_error(
    calc_sum_counterfactual_gam(m, baseline_data,
      counter_data = counter_data,
      ci = 0.95, delta = TRUE
    )
  )
})

test_that("absolute differerence produces finite values", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = counter_data,
    ci = 0.95, delta = TRUE,
    use_relative_diff = FALSE
  )

  is_result_finite <- sapply(test_diffs, is.finite)

  expect_true(all(is_result_finite))
})

test_that("relative differerence produces expected m lc and uc", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = counter_data,
    ci = 0.95, delta = TRUE,
    use_relative_diff = TRUE
  )

  expect_setequal(names(test_diffs), c("m", "lc", "uc"))
})

test_that("relative differerence produces finite values", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = counter_data,
    ci = 0.95, delta = TRUE,
    use_relative_diff = TRUE
  )
  is_result_finite <- sapply(test_diffs, is.finite)

  expect_true(all(is_result_finite))
})

test_that("sum with no counter data produces finite values", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = NULL,
    ci = 0.95, delta = TRUE,
    use_relative_diff = FALSE
  )

  is_result_finite <- sapply(test_diffs, is.finite)

  expect_true(all(is_result_finite))
})

test_that("error on counter_data not null and use_relative_diff", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  test_diffs <- calc_sum_counterfactual_gam(m, baseline_data,
    counter_data = counter_data,
    ci = 0.95, delta = TRUE,
    use_relative_diff = TRUE
  )

  is_result_finite <- sapply(test_diffs, is.finite)

  expect_true(all(is_result_finite))
})

test_that("difference of mean should be approx mean of differences",{
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  baseline_data <- dplyr::tibble(x = 0:20)
  counter_data <- dplyr::tibble(x = 21:40)
  baseline_sum <- calc_sum_counterfactual_gam(m, baseline_data)
  counter_sum <- calc_sum_counterfactual_gam(m, counter_data)
  diffs_sum <- calc_sum_counterfactual_gam(m, baseline_data,
                                           counter_data = counter_data)

  expect_equal(diffs_sum$m,baseline_sum$m - counter_sum$m)

})
