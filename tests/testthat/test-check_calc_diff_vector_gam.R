test_that("diff vector gam returns m lc and uc", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  newdata <- dplyr::tibble(x = c(0, 20, 60))
  test_diffs <- calc_diff_vector_gam(m, newdata, ci = 0.95)

  expect_setequal(names(test_diffs), c("m", "lc", "uc"))
})

test_that("diff vector gam returns vector of correct length", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  newdata <- dplyr::tibble(x = c(0, 20, 60))
  test_diffs <- calc_diff_vector_gam(m, newdata, ci = 0.95)

  expect_true(all(lengths(test_diffs) == 2))
})

test_that("diff vector gam returns all numeric", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  newdata <- dplyr::tibble(x = c(0, 20, 60))
  test_diffs <- calc_diff_vector_gam(m, newdata, ci = 0.95)

  expect_true(all(sapply(test_diffs, is.numeric)))
})

test_that("diff vector gam returns generates output for relative diff", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  newdata <- dplyr::tibble(x = c(0, 20, 60))
  test_diffs <- calc_diff_vector_gam(m, newdata, ci = 0.95,
                                     use_relative_diff = TRUE)

  expect_true(all(sapply(test_diffs, is.numeric)))
})

test_that("diff vector gam can use data.frame as input", {
  res <- create_random_data()
  m <- mgcv::gam(y ~ s(x), data = res, family = poisson)
  newdata <- data.frame(x = c(0, 20, 60))
  test_diffs <- calc_diff_vector_gam(m, newdata, ci = 0.95,
                                     use_relative_diff = TRUE)

  expect_true(all(sapply(test_diffs, is.numeric)))
})
