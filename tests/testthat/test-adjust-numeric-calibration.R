skip_if_not_installed("probably")

test_that("basic adjust_numeric_calibration usage works", {
  skip_if_not_installed("mgcv")

  library(tibble)

  set.seed(1)
  d_calibration <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() %>%
      adjust_numeric_calibration(method = "linear")
  )

  expect_no_condition(
    tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_test), colnames(tlr_pred))

  # calculations match those done manually
  # TODO: write out the probably code manually here
})

test_that("adjust_numeric_calibration() respects `method` argument", {
  library(tibble)

  set.seed(1)
  d_calibration <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))

  expect_no_condition(
    tlr <-
      tailor() %>%
      adjust_numeric_calibration(method = "isotonic")
  )

  expect_no_condition(
    tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_test), colnames(tlr_pred))

  # probably actually used an isotonic calibrator
  expect_equal(tlr_fit$adjustments[[1]]$results$fit$method, "Isotonic regression")
})

test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_numeric_calibration())
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(tailor(), "boop"))
  expect_snapshot(
    error = TRUE,
    tailor() %>% adjust_numeric_calibration("binary")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})

test_that("tunable", {
  tlr <-
    tailor() %>%
    adjust_numeric_calibration(method = "linear")
  adj_param <- tunable(tlr$adjustments[[1]])
  expect_equal(adj_param, no_param)
})
