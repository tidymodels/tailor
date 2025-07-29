skip_if_not_installed("probably")

test_that("basic adjust_numeric_calibration usage works", {
  skip_if_not_installed("mgcv")

  set.seed(1)
  d_calibration <- tibble::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_numeric_calibration(method = "linear")
  )

  expect_no_warning(
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
  set.seed(1)
  d_calibration <- tibble::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  expect_no_condition(
    tlr <-
      tailor() |>
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
  expect_equal(
    tlr_fit$adjustments[[1]]$results$fit$method,
    "Isotonic regression calibration"
  )
})

test_that("adjustment printing", {
  expect_snapshot(
    tailor() |> adjust_numeric_calibration()
  )
  expect_snapshot(
    tailor() |> adjust_numeric_calibration(method = "isotonic")
  )
  expect_snapshot(
    tailor() |> adjust_numeric_calibration(method = hardhat::tune())
  )

  expect_snapshot(
    tailor() |>
      adjust_numeric_calibration() |>
      fit(mtcars, outcome = mpg, estimate = disp)
  )
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(tailor(), "boop"))
  expect_snapshot(
    error = TRUE,
    tailor() |> adjust_numeric_calibration("binary")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})

test_that("tunable S3 method", {
  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "linear")
  adj_param <- tunable(tlr$adjustments[[1]])
  exp_tunable <-
    tibble::tibble(
      name = "method",
      call_info = list(list(pkg = "dials", fun = "cal_method_reg")),
      source = "tailor",
      component = "numeric_calibration",
      component_id = "numeric_calibration"
    )
  expect_equal(adj_param, exp_tunable)
})


test_that("tuning the calibration method", {
  set.seed(1)
  d_calibration <- tibble::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = hardhat::tune())
  expect_true(tailor:::is_tune(tlr$adjustments[[1]]$arguments$method))

  expect_snapshot(
    fit(tlr, d_calibration, outcome = y, estimate = y_pred),
    error = TRUE
  )
})

test_that("passing arguments to adjust_numeric_calibration", {
  set.seed(1)
  d_calibration <- tibble::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  expect_no_condition(
    tlr_fit <-
      tailor() |>
      adjust_numeric_calibration(method = "linear", smooth = FALSE) |>
      fit(d_calibration, outcome = y, estimate = y_pred)
  )

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    "cal_estimate_linear"
  )

  expect_snapshot(
    tlr_fit <-
      tailor() |>
      adjust_numeric_calibration(method = "linear", FALSE),
    error = TRUE
  )

  expect_snapshot(
    tlr_fit <-
      tailor() |>
      adjust_numeric_calibration(method = "linear", FALSE, select = TRUE),
    error = TRUE
  )
})


test_that("required packages for adjust_numeric_calibration", {
  skip_if_not_installed("mgcv")

  set.seed(1)
  d_calibration <- tibble::tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "linear")

  expect_no_warning(
    tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)
  )

  expect_equal(required_pkgs(tlr), c("probably", "tailor"))
  expect_equal(required_pkgs(tlr_fit), c("mgcv", "probably", "tailor"))

})
