test_that("adjustment printing", {
  dummy_reg_cal <- structure(list(), class = "cal_regression")
  expect_snapshot(ctr_reg %>% adjust_numeric_calibration(dummy_reg_cal))
})

test_that("errors informatively with bad input", {
  # check for `adjust_numeric_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_reg))
  expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_reg, "boop"))
  dummy_cls_cal <- structure(list(), class = "cal_binary")
  expect_snapshot(error = TRUE, adjust_numeric_calibration(ctr_cls, dummy_cls_cal))
})
