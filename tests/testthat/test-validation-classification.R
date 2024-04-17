test_that("validation of classification operations", {

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
