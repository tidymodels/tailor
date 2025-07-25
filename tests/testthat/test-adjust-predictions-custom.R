test_that("basic adjust_predictions_custom() usage works", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() |>
      adjust_predictions_custom(linear_predictor = binomial()$linkfun(Class2))
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      two_class_example,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )
  expect_no_condition(
    tlr_pred <- predict(tlr_fit, two_class_example)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(
    c(colnames(two_class_example), "linear_predictor"),
    colnames(tlr_pred)
  )

  # calculations match those done manually
  expect_equal(
    tlr_pred$linear_predictor,
    binomial()$linkfun(two_class_example$Class2)
  )
})

test_that("adjust_predictions_custom() for numerics works without setting type (#61)", {
  skip_if_not_installed("probably")

  library(tibble)

  set.seed(1)
  d_calibration <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  expect_no_error({
    tlr <-
      tailor() |>
      adjust_numeric_calibration() |>
      adjust_numeric_range(lower_limit = 2) |>
      adjust_predictions_custom(squared = y_pred^2)

    tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)
  })
})

test_that("adjustment printing", {
  expect_snapshot(tailor() |> adjust_predictions_custom())

  skip_if_not_installed("modeldata")
  data("two_class_example", package = "modeldata")

  expect_snapshot(
    tailor() |>
      adjust_predictions_custom(
        linear_predictor = binomial()$linkfun(Class2)
      ) |>
      fit(
        two_class_example,
        outcome = c(truth),
        estimate = c(predicted),
        probabilities = c(Class1, Class2)
      )
  )
})

test_that("tunable", {
  tlr <-
    tailor() |>
    adjust_predictions_custom(linear_predictor = binomial()$linkfun(Class2))
  adj_param <- tunable(tlr$adjustments[[1]])
  expect_equal(adj_param, no_param)
})
