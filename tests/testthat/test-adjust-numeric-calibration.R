test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(container()))
  expect_snapshot(error = TRUE, adjust_numeric_calibration(container(), "boop"))
  dummy_cls_cal <- structure(list(), class = "cal_binary")
  expect_snapshot(
    error = TRUE,
    adjust_numeric_calibration(container(), dummy_cls_cal)
  )
})
