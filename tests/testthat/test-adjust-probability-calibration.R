test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_probability_calibration("logistic"))
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(tailor)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(tailor(), "boop"))
  expect_snapshot(
    error = TRUE,
    tailor("regression") %>% adjust_probability_calibration("binary")
  )
  expect_snapshot(
    error = TRUE,
    tailor("binary") %>% adjust_probability_calibration("linear")
  )

  expect_no_condition(adjust_numeric_calibration(tailor()))
  expect_no_condition(adjust_numeric_calibration(tailor(), "linear"))
})
