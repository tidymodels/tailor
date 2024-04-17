test_that("validation of regression operations", {

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
