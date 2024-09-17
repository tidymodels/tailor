test_that("validation of adjustments (regression)", {
  expect_no_condition(
    reg_tailor <-
      tailor() %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_numeric_calibration() %>%
      adjust_predictions_custom(squared = .pred^2)
  )

  # todo should we error if a mutate occurs beforehand? Can we detect if it
  # modifies the prediction?
  expect_no_condition(
    reg_tailor <-
      tailor() %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2)
  )
})

test_that("validation of adjustments (classification)", {
  expect_no_condition(
    cls_tailor_1 <-
      tailor() %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_no_condition(
    cls_tailor_2 <-
      tailor() %>%
      adjust_predictions_custom(starch = "potato") %>%
      adjust_predictions_custom(veg = "green beans") %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_probability_threshold(threshold = .5) %>%
      adjust_probability_calibration()
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_equivocal_zone(value = .2) %>%
      adjust_probability_threshold(threshold = .4)
  )
})

test_that("validation of adjustments (ambiguous type)", {
  expect_no_condition(
    ambiguous_tailor <-
      tailor() %>%
      adjust_predictions_custom(squared = .pred^2) %>%
      adjust_predictions_custom(boop = boop)
  )

  expect_equal(ambiguous_tailor$type, "unknown")
})

test_that("validation of adjustments (incompatible types)", {
  # one bad adjustment each
  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_numeric_calibration() %>%
      adjust_probability_threshold()
  )

  # varying the pluralization...
  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_numeric_range(lower_limit = 2)
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_probability_threshold(threshold = .4)
  )

  # ensure that mixing in ambiguous adjustments doesn't cause issues
  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_numeric_calibration() %>%
      adjust_probability_threshold()
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_probability_calibration("logistic") %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_numeric_range(lower_limit = 2)
  )

  expect_snapshot(
    error = TRUE,
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_numeric_calibration() %>%
      adjust_numeric_range(lower_limit = 2) %>%
      adjust_probability_threshold(threshold = .4)
  )

  expect_no_condition(
    tailor() %>%
      adjust_predictions_custom(veg = "potato") %>%
      adjust_numeric_calibration()
  )

  expect_no_condition(
    tailor() %>%
      adjust_numeric_calibration() %>%
      adjust_predictions_custom(veg = "potato")
  )

  expect_no_condition(
    tailor() %>%
      adjust_probability_threshold(threshold = .4) %>%
      adjust_predictions_custom(veg = "potato")
  )
})
