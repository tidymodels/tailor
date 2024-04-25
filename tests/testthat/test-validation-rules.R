test_that("validation of operations (regression)", {
  dummy_reg_cal <- list()
  class(dummy_reg_cal) <- "cal_regression"

  expect_silent(
    reg_ctr <-
      container(mode = "regression") %>%
      adjust_numeric_calibration(dummy_reg_cal) %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  expect_snapshot(
    container(mode = "regression") %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_numeric_calibration(dummy_reg_cal) %>%
      adjust_predictions_custom(squared = .pred^2),
    error = TRUE
  )

  # todo should we error if a mutate occurs beforehand? Can we detect if it
  # modifies the prediction?
  expect_silent(
    reg_ctr <-
      container(mode = "regression") %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_numeric_calibration(dummy_reg_cal) %>%
      adjust_numeric_range(lower_limit = 2)
  )
})

test_that("validation of operations (classification)", {
  dummy_cls_cal <- list()
  class(dummy_cls_cal) <- "cal_binary"

  expect_silent(
    cls_ctr_1 <-
      container(mode = "classification") %>%
      adjust_probability_calibration(dummy_cls_cal) %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_silent(
    cls_ctr_2 <-
      container(mode = "classification") %>%
      adjust_predictions_custom(starch = "potato") %>%
      adjust_predictions_custom(veg = "green beans") %>%
      adjust_probability_calibration(dummy_cls_cal) %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration(dummy_cls_cal),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration(dummy_cls_cal),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_threshold(threshold = .5) %>%
      adjust_probability_calibration(dummy_cls_cal),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_equivocal_zone(value = .2) %>%
      adjust_probability_threshold(threshold = .4),
    error = TRUE
  )
})
