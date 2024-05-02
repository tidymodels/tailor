test_that("validation of operations (regression)", {
  expect_no_condition(
    reg_ctr <-
      container(type = "regression") %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  expect_snapshot(
    error = TRUE,
    container(type = "regression") %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_numeric_calibration() %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  # todo should we error if a mutate occurs beforehand? Can we detect if it
  # modifies the prediction?
  expect_no_condition(
    reg_ctr <-
      container(type = "regression") %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2)
  )
})

test_that("validation of operations (classification)", {
  expect_no_condition(
    cls_ctr_1 <-
      container(type = "binary") %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_no_condition(
    cls_ctr_2 <-
      container(type = "binary") %>%
      adjust_predictions_custom(starch = "potato") %>%
      adjust_predictions_custom(veg = "green beans") %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_snapshot(
    error = TRUE,
    container(type = "binary") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container() %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container(type = "binary") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container(type = "binary") %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_threshold(threshold = .5) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_threshold(threshold = .5) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    container(type = "binary") %>%
      adjust_equivocal_zone(value = .2) %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_snapshot(
    error = TRUE,
    container() %>%
      adjust_equivocal_zone(value = .2) %>%
      adjust_probability_threshold(threshold = .4)
  )
})

test_that("validation of operations (ambiguous type)", {
  expect_no_condition(
    ambiguous_ctr <-
      container() %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_predictions_custom(boop = boop)
  )

  expect_equal(ambiguous_ctr$type, "unknown")
})
