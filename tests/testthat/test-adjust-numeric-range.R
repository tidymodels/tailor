test_that("basic adjust_numeric_range() usage works", {
  library(tibble)

  # create example data
  set.seed(1)
  d <- tibble(y = rnorm(100), y_pred = y/2 + rnorm(100))

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() %>%
      adjust_numeric_range(lower_limit = 1, upper_limit = 2)
  )

  expect_no_condition(
    tlr_fit <- fit(tlr, d, outcome = y, estimate = y_pred)
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, d)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")

  # column names are as expected
  expect_equal(colnames(d), colnames(tlr_pred))

  # calculations match those done manually
  manual_preds <- ifelse(d$y_pred < 1, 1, d$y_pred)
  manual_preds <- ifelse(manual_preds > 2, 2, manual_preds)
  expect_equal(manual_preds, tlr_pred$y_pred)
})

test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_numeric_range())
  expect_snapshot(tailor() %>% adjust_numeric_range(hardhat::tune()))
  expect_snapshot(tailor() %>% adjust_numeric_range(-1, hardhat::tune()))
  expect_snapshot(tailor() %>% adjust_numeric_range(hardhat::tune(), 1))
})

test_that("tunable", {
  tlr <-
    tailor() %>%
    adjust_numeric_range(lower_limit = 1, upper_limit = 2)
  adj_param <- tunable(tlr$adjustments[[1]])
  expect_equal(adj_param$name, c("lower_limit", "upper_limit"))
  expect_true(all(adj_param$source == "tailor"))
  expect_true(is.list(adj_param$call_info))
  expect_equal(nrow(adj_param), 2)
  expect_equal(
    names(adj_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})
