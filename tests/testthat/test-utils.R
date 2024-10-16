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

test_that("find_tune_id() works", {
  # empty input
  expect_equal(find_tune_id(list()), NA_character_)

  # handles quosures
  x <- rlang::quos(a = 1, b = tune())
  expect_equal(find_tune_id(x), "")

  # non-tunable atomic values
  expect_equal(find_tune_id(1), NA_character_)
  expect_equal(find_tune_id("a"), NA_character_)
  expect_equal(find_tune_id(TRUE), NA_character_)

  # non-tunable names
  expect_equal(find_tune_id(quote(x)), NA_character_)

  # nested lists
  x <- list(a = 1, b = list(c = hardhat::tune(), d = 2))
  expect_equal(find_tune_id(x), "")

  # tune() without id
  expect_equal(find_tune_id(hardhat::tune()), "")

  # tune() with id
  expect_equal(find_tune_id(hardhat::tune("test_id")), "test_id")

  # multiple tunable values
  x <- list(a = hardhat::tune(), b = hardhat::tune())
  expect_snapshot(error = TRUE, find_tune_id(x))
})
