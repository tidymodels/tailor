test_that("adjustment printing", {
  dummy_cls_cal <- structure(list(), class = "cal_binary")
  expect_snapshot(ctr_cls %>% adjust_probability_calibration(dummy_cls_cal))
})

test_that("errors informatively with bad input", {
  # check for `adjust_probably_calibration(container)` is in `utils.R` tests

  expect_snapshot(error = TRUE, adjust_probability_calibration(ctr_cls))
  expect_snapshot(error = TRUE, adjust_probability_calibration(ctr_cls, "boop"))
  dummy_reg_cal <- structure(list(), class = "cal_regression")
  expect_snapshot(
    error = TRUE,
    adjust_probability_calibration(ctr_cls, dummy_reg_cal)
  )
})
