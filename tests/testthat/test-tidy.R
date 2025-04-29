skip_if_not_installed("probably")

test_that("tidy.tailor works", {
  library(tibble)

  set.seed(1)
  d_calibration <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))
  d_test <- tibble(y = rnorm(100), y_pred = y / 2 + rnorm(100))

  # TODO: reintroduce custom predictions when #61 is resolved
  tlr <-
    tailor() %>%
    adjust_numeric_calibration() %>%
    adjust_numeric_range(lower_limit = 2) #%>%
  #adjust_predictions_custom(squared = y_pred^2)

  tidy_tlr <- tidy(tlr)

  expect_s3_class(tidy_tlr, "tbl_df")
  expect_equal(nrow(tidy_tlr), length(tlr$adjustments))
  expect_named(
    tidy_tlr,
    c(
      "number",
      "name",
      "input",
      "output_numeric",
      "output_prob",
      "output_class",
      "output_all",
      "trained",
      "requires_training"
    )
  )
  expect_equal(tidy_tlr$number, seq_len(length(tlr$adjustments)))
  expect_false(any(tidy_tlr$trained))
  expect_true(any(tidy_tlr$requires_training))

  tidy_tlr_1 <- tidy(tlr, 1)
  tidy_tlr_2 <- tidy(tlr, 2)

  expect_equal(tidy_tlr[1, ], tidy_tlr_1)
  expect_equal(tidy_tlr[2, ], tidy_tlr_2)

  tlr_fit <- fit(tlr, d_calibration, outcome = y, estimate = y_pred)

  tidy_tlr_fit <- tidy(tlr_fit)

  expect_identical(
    tidy_tlr[names(tidy_tlr) != "trained"],
    tidy_tlr_fit[names(tidy_tlr_fit) != "trained"]
  )
  expect_true(all(tidy_tlr_fit$trained))
})

test_that("tidy.tailor errors informatively with bad arguments", {
  tlr <-
    tailor() %>%
    adjust_numeric_calibration() %>%
    adjust_numeric_range(lower_limit = 2)

  expect_error(tidy(tlr, silly = "head"), class = "rlib_error_dots_nonempty")
  expect_snapshot(error = TRUE, tidy(tlr, number = 4))
})

test_that("tidying a tailor with no adjustments", {
  tidy_tlr <- tidy(tailor())

  expect_equal(nrow(tidy_tlr), 0)
  expect_equal(
    ncol(tidy_tlr),
    tailor() %>%
      adjust_numeric_calibration() %>%
      tidy() %>%
      ncol()
  )
})
