skip_if_not_installed("probably")

test_that("basic adjust_numeric_calibration usage works", {
  skip_if_not_installed("mgcv")

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_numeric_calibration(method = "linear")
  )

  expect_no_warning(
    tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_reg_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_reg_test), colnames(tlr_pred))
})

test_that("linear adjust_numeric_calibration usage works", {
  skip_if_not_installed("mgcv")

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "linear")

  tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  tlr_pred <- predict(tlr_fit, d_reg_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_linear_spline", "cal_regression", "cal_object")
  )
  expect_true(all(d_reg_test$y_pred != tlr_pred$y_pred))
})

test_that("isotonic adjust_numeric_calibration usage works", {
   tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "isotonic")

  tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  tlr_pred <- predict(tlr_fit, d_reg_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic", "cal_regression", "cal_object")
  )
  expect_true(all(d_reg_test$y_pred != tlr_pred$y_pred))
})


test_that("isotonic boot adjust_numeric_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "isotonic_boot")

  set.seed(1)
  tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  tlr_pred <- predict(tlr_fit, d_reg_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic_boot", "cal_regression", "cal_object")
  )
  expect_true(all(d_reg_test$y_pred != tlr_pred$y_pred))
})

test_that("no adjust_numeric_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "none")

  set.seed(1)
  tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  tlr_pred <- predict(tlr_fit, d_reg_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_none", "cal_regression", "cal_object")
  )
  expect_true(all(d_reg_test$y_pred == tlr_pred$y_pred))
})

test_that("adjust_numeric_calibration() respects `method` argument", {

  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_numeric_calibration(method = "isotonic")
  )

  expect_no_condition(
    tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_reg_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_reg_test), colnames(tlr_pred))

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

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = hardhat::tune())
  expect_true(tailor:::is_tune(tlr$adjustments[[1]]$arguments$method))

  expect_snapshot(
    fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred),
    error = TRUE
  )
})

test_that("too few data", {
  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "linear")

  expect_snapshot(
    fit(tlr, d_reg_calibration[0,], outcome = y, estimate = y_pred)
  )
  expect_snapshot(
    fit(tlr, d_reg_calibration[1,], outcome = y, estimate = y_pred)
  )

})

test_that("passing arguments to adjust_numeric_calibration", {

  expect_no_condition(
    tlr_fit <-
      tailor() |>
      adjust_numeric_calibration(method = "linear", smooth = FALSE) |>
      fit(d_reg_calibration, outcome = y, estimate = y_pred)
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

test_that("harden against calibration model failure", {
  skip_if(getRversion() < "4.2.0")

  set.seed(1)
  d_calibration_pred <- tibble(y = rnorm(100), y_pred = NA_real_)
  d_calibration_y <- tibble(y = NA_real_, y_pred = rnorm(100))

  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  tlr <-
    tailor() |>
    adjust_numeric_calibration(method = "linear")

  ###
  expect_snapshot(
    pred_fit <- fit(tlr, d_calibration_pred, outcome = y, estimate = y_pred)
  )

  pred_pred <- predict(pred_fit, d_test)

  expect_true(all(pred_pred$y_pred == d_test$y_pred))

  ###
  expect_snapshot(
    y_fit <- fit(tlr, d_calibration_y, outcome = y, estimate = y_pred)
  )

  y_pred <- predict(y_fit, d_test)

  expect_true(all(y_pred$y_pred == d_test$y_pred))
})

test_that("required packages for adjust_numeric_calibration", {
  skip_if_not_installed("mgcv")

  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_numeric_calibration(method = "linear")
  )

  expect_no_warning(
    tlr_fit <- fit(tlr, d_reg_calibration, outcome = y, estimate = y_pred)
  )

  expect_equal(required_pkgs(tlr), c("probably", "tailor"))
  expect_equal(required_pkgs(tlr_fit), c("mgcv", "probably", "tailor"))

})
