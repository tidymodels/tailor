test_that("adjustment printing", {
  expect_snapshot(container() %>% adjust_numeric_calibration())
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(container(), "boop"))
  expect_snapshot(
    error = TRUE,
    container("binary") %>% adjust_numeric_calibration("linear")
  )
  expect_snapshot(
    error = TRUE,
    container("regression") %>% adjust_numeric_calibration("binary")
  )

  expect_no_condition(adjust_numeric_calibration(container()))
  expect_no_condition(adjust_numeric_calibration(container(), "linear"))
})
