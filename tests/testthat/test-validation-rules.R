test_that("validation of operations (regression)", {
  expect_silent(
    reg_ctr <-
      container(mode = "regression") %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  expect_snapshot(
    container(mode = "regression") %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_numeric_calibration() %>%
      adjust_predictions_custom(squared = .pred^2),
    error = TRUE
  )

  # todo should we error if a mutate occurs beforehand? Can we detect if it
  # modifies the prediction?
  expect_silent(
    reg_ctr <-
      container(mode = "regression") %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2)
  )
})

test_that("validation of operations (classification)", {
  expect_silent(
    cls_ctr_1 <-
      container(mode = "classification") %>%
      # to-do: should be able to supply no `type` argument here
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_silent(
    cls_ctr_2 <-
      container(mode = "classification") %>%
      adjust_predictions_custom(starch = "potato") %>%
      adjust_predictions_custom(veg = "green beans") %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration(),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration(),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_threshold(threshold = .5) %>%
      adjust_probability_calibration(),
    error = TRUE
  )

  expect_snapshot(
    container(mode = "classification") %>%
      adjust_equivocal_zone(value = .2) %>%
      adjust_probability_threshold(threshold = .4),
    error = TRUE
  )
})
