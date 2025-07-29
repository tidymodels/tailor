skip_if_not_installed("probably")

test_that("basic adjust_probability_calibration() usage works", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("mgcv")

  library(modeldata)

  # split example data
  set.seed(1)
  in_rows <- sample(c(TRUE, FALSE), nrow(two_class_example), replace = TRUE)
  d_calibration <- two_class_example[in_rows, ]
  d_test <- two_class_example[!in_rows, ]

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "logistic")
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      d_calibration,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
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
  # TODO: write out the manual code with probably
})

test_that("basic adjust_probability_calibration() usage works", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  # split example data
  set.seed(1)
  in_rows <- sample(c(TRUE, FALSE), nrow(two_class_example), replace = TRUE)
  d_calibration <- two_class_example[in_rows, ]
  d_test <- two_class_example[!in_rows, ]

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "isotonic")
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      d_calibration,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
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
    tailor() |> adjust_probability_calibration()
  )
  expect_snapshot(
    tailor() |> adjust_probability_calibration(method = "logistic")
  )
  expect_snapshot(
    tailor() |> adjust_probability_calibration(method = hardhat::tune())
  )

  skip_if_not_installed("modeldata")
  data("two_class_example", package = "modeldata")

  expect_snapshot(
    tailor() |>
      adjust_probability_calibration() |>
      fit(
        two_class_example,
        outcome = c(truth),
        estimate = c(predicted),
        probabilities = c(Class1, Class2)
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
  skip_if_not_installed("modeldata")
  library(modeldata)

  # split example data
  set.seed(1)
  in_rows <- sample(c(TRUE, FALSE), nrow(two_class_example), replace = TRUE)
  d_calibration <- two_class_example[in_rows, ]
  d_test <- two_class_example[!in_rows, ]

  tlr <-
    tailor() |>
    adjust_probability_calibration(method = hardhat::tune())
  expect_true(tailor:::is_tune(tlr$adjustments[[1]]$arguments$method))

  expect_snapshot(
    fit(
      tlr,
      d_calibration,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    ),
    error = TRUE
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

  library(modeldata)

  # split example data
  set.seed(1)
  in_rows <- sample(c(TRUE, FALSE), nrow(two_class_example), replace = TRUE)
  d_calibration <- two_class_example[in_rows, ]
  d_test <- two_class_example[!in_rows, ]

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_probability_calibration(method = "logistic")
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      d_calibration,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )

  expect_equal(required_pkgs(tlr), c("probably", "tailor"))
  expect_equal(required_pkgs(tlr_fit), c("mgcv", "probably", "tailor"))

})

