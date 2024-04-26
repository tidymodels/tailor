test_that("adjustment printing", {
  expect_snapshot(ctr_cls %>% adjust_probability_calibration("logistic"))
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(ctr_cls, "boop"))
  expect_snapshot(
    error = TRUE,
    container("regression", "regression") %>% adjust_probability_calibration("binary")
  )
  # todo: this should error, all is fine besides the fn name
  # expect_snapshot(
  #   container("regression", "regression") %>% adjust_probability_calibration("linear")
  # )
  # todo: this should error, mode is incompatible even though type is fine
  # expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_cls, "linear"))

  expect_no_condition(adjust_numeric_calibration(ctr_reg))
  expect_no_condition(adjust_numeric_calibration(ctr_reg, "linear"))
})
