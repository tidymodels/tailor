test_that("check_tailor raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(tailor() %>% adjust_probability_threshold(.5))
})

test_that("tailor_fully_trained works", {
  skip_if_not_installed("modeldata")
  data("two_class_example", package = "modeldata")
  expect_false(tailor_fully_trained(tailor()))
  expect_false(
    tailor_fully_trained(tailor() %>% adjust_probability_threshold(.5))
  )
  expect_false(
    tailor_fully_trained(
      tailor() %>%
        adjust_probability_calibration("logistic") %>%
        fit(
          two_class_example,
          outcome = "truth",
          estimate = tidyselect::contains("Class"),
          probabilities = tidyselect::contains("Class")
        ) %>%
        adjust_probability_threshold(.5)
    )
  )

  expect_true(
    tailor_fully_trained(
      tailor() %>%
        adjust_probability_calibration("logistic") %>%
        fit(
          two_class_example,
          outcome = "truth",
          # todo: this function requires a different format of `estimate`
          # and `probabilities` specification than the call below to
          # be able to fit properly.
          estimate = tidyselect::contains("Class")
        )
    )
  )
  expect_true(
    tailor_fully_trained(
      tailor() %>%
        adjust_probability_threshold(.5) %>%
        fit(
          two_class_example,
          outcome = "truth",
          estimate = "predicted",
          probabilities = tidyselect::contains("Class")
        )
    )
  )
})


test_that("tailor_requires_fit works", {
  expect_false(tailor_requires_fit(tailor()))
  expect_false(
    tailor_requires_fit(tailor() %>% adjust_probability_threshold(.5))
  )
  expect_true(
    tailor_requires_fit(
      tailor() %>%
        adjust_probability_calibration("logistic")
    )
  )
  expect_true(
    tailor_requires_fit(
      tailor() %>%
        adjust_probability_calibration("logistic") %>%
        adjust_probability_threshold(.5)
    )
  )
})
