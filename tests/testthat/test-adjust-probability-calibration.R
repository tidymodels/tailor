test_that("adjustment printing", {
  expect_snapshot(container() %>% adjust_probability_calibration("logistic"))
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(container(), "boop"))
  expect_snapshot(
    error = TRUE,
    container("regression") %>% adjust_probability_calibration("binary")
  )
  expect_snapshot(
    error = TRUE,
    container("binary") %>% adjust_probability_calibration("linear")
  )

  expect_no_condition(adjust_numeric_calibration(container()))
  expect_no_condition(adjust_numeric_calibration(container(), "linear"))
})
