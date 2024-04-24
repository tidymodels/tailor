test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(container()))
  expect_snapshot(error = TRUE, adjust_probability_calibration(container(), "boop"))
  dummy_reg_cal <- structure(list(), class = "cal_regressions")
  expect_snapshot(
    error = TRUE,
    adjust_probability_calibration(container(), dummy_reg_cal)
  )
})
