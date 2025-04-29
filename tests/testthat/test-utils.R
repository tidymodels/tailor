test_that("is_tune works", {
  expect_false(is_tune(1))
  expect_false(is_tune("x"))
  expect_false(is_tune(quote(x)))
  expect_false(is_tune(quote(f(x))))
  expect_false(is_tune(NULL))
  expect_false(is_tune(list()))

  expect_true(is_tune(quote(tune())))
  expect_true(is_tune(quote(tune("my_param"))))
})

test_that("check_tailor raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(tailor() %>% adjust_probability_threshold(.5))
})

test_that("check_calibration_type errors informatively", {
  expect_no_error(check_calibration_type("numeric", "numeric", "regression"))
  expect_no_error(
    check_calibration_type("probability", "probability", "binary")
  )
  expect_no_error(
    check_calibration_type("probability", "probability", "multiclass")
  )

  expect_snapshot(
    error = TRUE,
    check_calibration_type("probability", "numeric", "regression")
  )

  expect_snapshot(
    error = TRUE,
    check_calibration_type("numeric", "probability", "binary")
  )

  expect_snapshot(
    error = TRUE,
    check_calibration_type("numeric", "probability", "multiclass")
  )
})

test_that("errors informatively without probably installed", {
  testthat::local_mocked_bindings(requireNamespace = function(...) {
    FALSE
  })

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
  skip_if_not_installed("probably")

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

test_that("fit.tailor() errors informatively with incompatible estimate", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  two_class_example$test_numeric <- two_class_example$Class1 + 1
  two_class_example$test_date <- as.POSIXct(two_class_example$Class1)

  # supply a numeric estimate to a binary tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_probability_threshold(.1),
      two_class_example,
      outcome = c(predicted),
      estimate = c(test_numeric),
      probabilities = c(Class1, Class2)
    )
  )

  # supply a factor estimate to a regression tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_numeric_range(lower_limit = .1),
      two_class_example,
      outcome = c(Class1),
      estimate = c(truth)
    )
  )

  # supply a totally wild estimate to a regression tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_probability_threshold(.1),
      two_class_example,
      outcome = c(truth),
      estimate = c(test_date),
      probabilities = c(Class1, Class2)
    )
  )

  # supply a totally wild estimate to an unknown tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_predictions_custom(hey = "there"),
      two_class_example,
      outcome = c(truth),
      estimate = c(test_date),
      probabilities = c(Class1)
    )
  )
})

test_that("fit.tailor() errors informatively with incompatible probability", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  two_class_example$test_date <- as.POSIXct(two_class_example$Class1)

  # supply a date probability to a binary tailor
  expect_snapshot(
    error = TRUE,
    fit(
      tailor() %>% adjust_probability_threshold(.1),
      two_class_example,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(test_date)
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

test_that("tune_id() works", {
  # works when input is tune
  expect_equal(tune_id(hardhat::tune()), "")
  expect_equal(tune_id(hardhat::tune("param")), "param")

  # returns character NA for non-tunable inputs
  expect_equal(tune_id(NULL), NA_character_)
  expect_equal(tune_id(1), NA_character_)
  expect_equal(tune_id("x"), NA_character_)
  expect_equal(tune_id(quote(x)), NA_character_)
  expect_equal(tune_id(quote(f(x))), NA_character_)
})

test_that("check_selection() errors informatively", {
  expect_snapshot(
    check_selection(quote(contains("boop")), numeric(0), ".data"),
    error = TRUE
  )
})
