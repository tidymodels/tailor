test_that("adjustment printing", {
  expect_snapshot(ctr_reg %>% adjust_numeric_calibration())
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_reg, "boop"))
  expect_snapshot(
    error = TRUE,
    container("classification", "binary") %>% adjust_numeric_calibration("linear")
  )
  expect_snapshot(
    error = TRUE,
    container("regression", "regression") %>% adjust_numeric_calibration("binary")
  )
  # todo: this should error, mode is incompatible even though type is fine
  # expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_cls, "linear"))

  expect_no_condition(adjust_numeric_calibration(ctr_reg))
  expect_no_condition(adjust_numeric_calibration(ctr_reg, "linear"))
})
