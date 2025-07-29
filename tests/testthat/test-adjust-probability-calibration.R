skip_if_not_installed("probably")

test_that("basic adjust_probability_calibration() usage works", {
  skip_if_not_installed("mgcv")

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "logistic")
  )

  # The first time executed, there may be a message "Registered S3 method
  # overwritten by 'butcher':"
  expect_no_warning(
    tlr_fit <- fit(
      tlr,
      d_bin_calibration,
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    )
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_bin_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_bin_test), colnames(tlr_pred))
})

test_that("basic adjust_probability_calibration() usage works", {
  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "isotonic")
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      d_bin_calibration,
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    )
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d_bin_test)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d_bin_test), colnames(tlr_pred))

  # probably actually used an isotonic calibrator
  expect_equal(
    tlr_fit$adjustments[[1]]$results$fit$method,
    "Isotonic regression calibration"
  )
})

test_that("logistic adjust_probability_calibration usage works", {
  skip_if_not_installed("mgcv")

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "logistic")

  tlr_fit <- fit(
    tlr,
    d_bin_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b)
  )
  tlr_pred <- predict(tlr_fit, d_bin_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_logistic_spline", "cal_binary", "cal_object")
  )
  expect_true(all(d_bin_test$a != tlr_pred$a))
  expect_true(all(d_bin_test$b != tlr_pred$b))
})

test_that("binary isotonic adjust_probability_calibration usage works", {
  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "isotonic")

  tlr_fit <- fit(
    tlr,
    d_bin_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b)
  )
  tlr_pred <- predict(tlr_fit, d_bin_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic", "cal_binary", "cal_object")
  )
  expect_true(all(d_bin_test$a != tlr_pred$a))
  expect_true(all(d_bin_test$b != tlr_pred$b))
})

test_that("binary isotonic boot adjust_probability_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "isotonic_boot")

  set.seed(1)
  tlr_fit <- fit(
    tlr,
    d_bin_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b)
  )
  tlr_pred <- predict(tlr_fit, d_bin_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic_boot", "cal_binary", "cal_object")
  )
  expect_true(all(d_bin_test$a != tlr_pred$a))
  expect_true(all(d_bin_test$b != tlr_pred$b))
})

test_that("binary beta adjust_probability_calibration usage works", {
  skip_if_not_installed("betacal")

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "beta")

  set.seed(1)
  tlr_fit <- fit(
    tlr,
    d_bin_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b)
  )
  tlr_pred <- predict(tlr_fit, d_bin_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_beta", "cal_binary", "cal_object")
  )
  expect_true(all(d_bin_test$a != tlr_pred$a))
  expect_true(all(d_bin_test$b != tlr_pred$b))
})

test_that("binary no adjust_probability_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "none")

  tlr_fit <- fit(
    tlr,
    d_bin_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b)
  )
  tlr_pred <- predict(tlr_fit, d_bin_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_none", "cal_binary", "cal_object")
  )
  expect_true(all(d_bin_test$a == tlr_pred$a))
  expect_true(all(d_bin_test$b == tlr_pred$b))
})


test_that("multinomial adjust_probability_calibration usage works", {
  skip_if_not_installed("mgcv")

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "multinomial")

  tlr_fit <- fit(
    tlr,
    d_mlt_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b, c)
  )
  tlr_pred <- predict(tlr_fit, d_mlt_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_multinomial_spline", "cal_estimate_multinomial",
      "cal_multi", "cal_object")
  )
  expect_true(all(d_mlt_test$a != tlr_pred$a))
  expect_true(all(d_mlt_test$b != tlr_pred$b))
  expect_true(all(d_mlt_test$c != tlr_pred$c))
})

test_that("multinomial isotonic adjust_probability_calibration usage works", {
  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "isotonic")

  tlr_fit <- fit(
    tlr,
    d_mlt_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b, c)
  )
  tlr_pred <- predict(tlr_fit, d_mlt_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic", "cal_multi", "cal_object")
  )
  expect_true(all(d_mlt_test$a != tlr_pred$a))
  expect_true(all(d_mlt_test$b != tlr_pred$b))
  expect_true(all(d_mlt_test$c != tlr_pred$c))
})

test_that("multinomial isotonic boot adjust_probability_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "isotonic_boot")

  set.seed(1)
  tlr_fit <- fit(
    tlr,
    d_mlt_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b, c)
  )
  tlr_pred <- predict(tlr_fit, d_mlt_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_isotonic_boot", "cal_multi", "cal_object")
  )
  expect_true(all(d_mlt_test$a != tlr_pred$a))
  expect_true(all(d_mlt_test$b != tlr_pred$b))
  expect_true(all(d_mlt_test$c != tlr_pred$c))
})

test_that("multinomial beta adjust_probability_calibration usage works", {
  skip_if_not_installed("betacal")

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "beta")

  set.seed(1)
  tlr_fit <- fit(
    tlr,
    d_mlt_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b, c)
  )
  tlr_pred <- predict(tlr_fit, d_mlt_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_beta", "cal_multi", "cal_object")
  )
  expect_true(all(d_mlt_test$a != tlr_pred$a))
  expect_true(all(d_mlt_test$b != tlr_pred$b))
  expect_true(all(d_mlt_test$c != tlr_pred$c))
})

test_that("multinomial no adjust_probability_calibration usage works", {

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "none")

  tlr_fit <- fit(
    tlr,
    d_mlt_calibration,
    outcome = c(y),
    estimate = c(predicted),
    probabilities = c(a, b, c)
  )
  tlr_pred <- predict(tlr_fit, d_mlt_test)

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    c("cal_estimate_none", "cal_multi", "cal_object")
  )
  expect_true(all(d_mlt_test$a == tlr_pred$a))
  expect_true(all(d_mlt_test$b == tlr_pred$b))
  expect_true(all(d_mlt_test$c == tlr_pred$c))
})

test_that("adjustment printing", {
  expect_snapshot(
    tailor() |> adjust_probability_calibration()
  )
  expect_snapshot(
    tailor() |> adjust_probability_calibration(method = "logistic")
  )
  expect_snapshot(
    tailor() |> adjust_probability_calibration(method = hardhat::tune())
  )
  expect_snapshot(
    tailor() |>
      adjust_probability_calibration() |>
      fit(
        d_bin_calibration,
        outcome = c(y),
        estimate = c(predicted),
        probabilities = c(a, b)
      )
  )
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(
    error = TRUE,
    adjust_probability_calibration(tailor(), "boop")
  )
  expect_snapshot(
    error = TRUE,
    tailor() |> adjust_probability_calibration("linear")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})

test_that("tunable S3 method", {
  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "logistic")
  adj_param <- tunable(tlr$adjustments[[1]])
  exp_tunable <-
    tibble::tibble(
      name = "method",
      call_info = list(list(pkg = "dials", fun = "cal_method_class")),
      source = "tailor",
      component = "probability_calibration",
      component_id = "probability_calibration"
    )
  expect_equal(adj_param, exp_tunable)
})

test_that("tuning the calibration method", {
  tlr <-
    tailor() |>
    adjust_probability_calibration(method = hardhat::tune())
  expect_true(tailor:::is_tune(tlr$adjustments[[1]]$arguments$method))

  expect_snapshot(
    fit(
      tlr,
      d_bin_calibration,
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    ),
    error = TRUE
  )
})


test_that("too few data", {
  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "logistic")

  expect_snapshot(
    tlr_fit <- fit(
      tlr,
      d_bin_calibration[0,],
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    )
  )
  expect_snapshot(
    tlr_fit <- fit(
      tlr,
      d_bin_calibration[1,],
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    )
  )

})


test_that("passing arguments to adjust_probability_calibration", {
  skip_if_not_installed("modeldata")

  library(modeldata)

  expect_no_condition(
    tlr_fit <-
      tailor() |>
      adjust_probability_calibration(method = "logistic", smooth = FALSE) |>
      fit(
        two_class_example,
        outcome = c(truth),
        estimate = c(predicted),
        probabilities = c(Class1, Class2)
      )
  )

  expect_s3_class(
    tlr_fit$adjustments[[1]]$results$fit,
    "cal_estimate_logistic"
  )

  expect_snapshot(
    tlr_fit <-
      tailor() |>
      adjust_probability_calibration(method = "logistic", FALSE),
    error = TRUE
  )

})

test_that("required packages for adjust_probability_calibration", {
  skip_if_not_installed("mgcv")

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "logistic")
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      d_bin_calibration,
      outcome = c(y),
      estimate = c(predicted),
      probabilities = c(a, b)
    )
  )

  expect_equal(required_pkgs(tlr), c("probably", "tailor"))
  expect_equal(required_pkgs(tlr_fit), c("mgcv", "probably", "tailor"))

})

test_that("harden against calibration model failure", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("betacal")

  library(modeldata)

  # split example data
  set.seed(1)
  d_y_calibration <-
    two_class_example |>
    dplyr::mutate(truth = rep(truth[1], nrow(two_class_example)))
  d_test <- two_class_example

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = "beta")

  ###
  expect_snapshot(
    y_fit <- fit(
      tlr,
      d_y_calibration,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )

  y_pred <- predict(y_fit, d_test)

  expect_true(all(y_pred$Class1 == d_test$Class1))
  expect_true(all(y_pred$Class2 == d_test$Class2))
  expect_true(all(y_pred$predicted == d_test$predicted))
})
