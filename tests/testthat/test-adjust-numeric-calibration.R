test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_numeric_calibration())
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(tailor(), "boop"))
  expect_snapshot(
    error = TRUE,
    tailor("binary") %>% adjust_numeric_calibration("linear")
  )
  expect_snapshot(
    error = TRUE,
    tailor("regression") %>% adjust_numeric_calibration("binary")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})
