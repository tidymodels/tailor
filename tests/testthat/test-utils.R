test_that("check_tailor raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(tailor() %>% adjust_probability_threshold(.5))
})

test_that("errors informatively without probably installed", {
  testthat::local_mocked_bindings(requireNamespace = function(...) {FALSE})

  expect_snapshot(error = TRUE, tailor() %>% adjust_numeric_calibration())
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
          estimate = predicted,
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
          estimate = predicted,
          probabilities = tidyselect::contains("Class")
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

test_that("fit.tailor() errors informatively with incompatible outcome", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  two_class_example$test_numeric <- two_class_example$Class1 + 1
  two_class_example$test_date <- as.POSIXct(two_class_example$Class1)

  # supply a numeric outcome to a binary tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_probability_threshold(.1),
      two_class_example,
      outcome = c(test_numeric),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )

  # supply a factor outcome to a regression tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_numeric_range(lower_limit = .1),
      two_class_example,
      outcome = c(truth),
      estimate = c(Class1)
    )
  )

  # supply a totally wild outcome to a regression tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_probability_threshold(.1),
      two_class_example,
      outcome = c(test_date),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )

  # supply a totally wild outcome to an unknown tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_predictions_custom(hey = "there"),
      two_class_example,
      outcome = c(test_date),
      estimate = c(predicted),
      probabilities = c(Class1)
    )
  )
})
