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
      tailor() %>%
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
      tailor() %>%
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
  expect_equal(tlr_fit$adjustments[[1]]$results$fit$method, "Isotonic regression")
})

test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_probability_calibration("logistic"))
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(tailor(), "boop"))
  expect_snapshot(
    error = TRUE,
    tailor() %>% adjust_probability_calibration("linear")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})

test_that("tunable", {
  tlr <-
    tailor() %>%
    adjust_probability_calibration(method = "logistic")
  adj_param <- tunable(tlr$adjustments[[1]])
  expect_equal(adj_param, no_param)
})
