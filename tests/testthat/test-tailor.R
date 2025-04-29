skip_if_not_installed("probably")

test_that("tailor printing", {
  expect_snapshot(tailor())
  expect_snapshot(tailor())
  expect_snapshot(
    tailor() |>
      adjust_probability_threshold(.2)
  )
  expect_snapshot(
    tailor() |>
      adjust_probability_threshold(.2) |>
      adjust_equivocal_zone()
  )
})

test_that("error informatively with empty tidyselections", {
  skip_if_not_installed("modeldata")
  data("two_class_example", package = "modeldata")

  expect_no_condition(
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = "truth",
        estimate = "predicted",
        probabilities = tidyselect::contains("Class")
      )
  )

  # outcome doesn't exist, is bare string
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = "truth_WRONG",
        estimate = "predicted",
        probabilities = tidyselect::contains("Class")
      )
  )

  # outcome doesn't exist, is selection helper
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = contains("truth_WRONG"),
        estimate = "predicted",
        probabilities = tidyselect::contains("Class")
      )
  )

  # estimate doesn't exist, is bare string
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = "truth",
        estimate = "predicted_WRONG",
        probabilities = tidyselect::contains("Class")
      )
  )

  # estimate doesn't exist, is selection helper
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = "truth",
        estimate = contains("predicted_WRONG"),
        probabilities = tidyselect::contains("Class")
      )
  )

  # probability doesn't exist, is selection helper, is needed
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_probability_threshold(.5) |>
      fit(
        two_class_example,
        outcome = contains("truth"),
        estimate = "predicted",
        probabilities = tidyselect::contains("Class_WRONG")
      )
  )

  # probability doesn't exist, is selection helper, isn't needed
  # (asserting here that we ought to error on a bad selection
  # if it would not be used anyway.)
  expect_snapshot(
    error = TRUE,
    tailor_fit <- tailor() |>
      adjust_numeric_range(.5) |>
      fit(
        two_class_example,
        outcome = "Class1",
        estimate = ".pred",
        probabilities = tidyselect::contains("Class_WRONG")
      )
  )
})

test_that("tunable (no adjustments)", {
  tlr <-
    tailor()

  tlr_param <- tunable(tlr)
  expect_equal(tlr_param, no_param)
})

test_that("tunable (multiple adjustments)", {
  tlr <-
    tailor() |>
    adjust_probability_threshold(.2) |>
    adjust_equivocal_zone()

  tlr_param <- tunable(tlr)
  expect_equal(tlr_param$name, c("threshold", "buffer"))
  expect_true(all(tlr_param$source == "tailor"))
  expect_true(is.list(tlr_param$call_info))
  expect_equal(nrow(tlr_param), 2)
  expect_equal(
    names(tlr_param),
    c("name", "call_info", "source", "component", "component_id")
  )

  expect_equal(
    tlr_param,
    dplyr::bind_rows(
      tunable(tlr$adjustments[[1]]),
      tunable(tlr$adjustments[[2]])
    )
  )
})
