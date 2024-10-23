test_that("extract parameter set from tailor with no adjustments", {
  skip_if_not_installed("dials")

  bare_tlr <- tailor()

  bare_info <- extract_parameter_set_dials(bare_tlr)
  check_parameter_set_tibble(bare_info)
  expect_equal(nrow(bare_info), 0)
})

test_that("extract parameter set from tailor with no tunable parameters", {
  skip_if_not_installed("dials")

  tlr <-
    tailor() %>%
    adjust_predictions_custom()

  tlr_info <- extract_parameter_set_dials(tlr)

  check_parameter_set_tibble(tlr_info)
  expect_equal(nrow(tlr_info), 0)
})

test_that("extract parameter set from tailor with a tunable parameter", {
  skip_if_not_installed("dials")

  tlr <-
    tailor() %>%
    adjust_numeric_calibration() %>%
    adjust_numeric_range(lower_limit = hardhat::tune())

  tlr_info <- extract_parameter_set_dials(tlr)

  check_parameter_set_tibble(tlr_info)
  expect_equal(nrow(tlr_info), 1)

  expect_equal(tlr_info$component, "numeric_range")
  expect_true(all(tlr_info$source == "tailor"))
  expect_equal(tlr_info$name, "lower_limit")
  expect_equal(tlr_info$id, "lower_limit")

  expect_equal(tlr_info$object[[1]], dials::lower_limit(c(-Inf, Inf)))
})

test_that("extract parameter set from tailor with multiple tunable parameters", {
  skip_if_not_installed("dials")

  tlr <-
    tailor() %>%
    adjust_numeric_calibration() %>%
    adjust_numeric_range(
      lower_limit = hardhat::tune(),
      upper_limit = hardhat::tune()
    )

  tlr_info <- extract_parameter_set_dials(tlr)

  check_parameter_set_tibble(tlr_info)
  expect_equal(nrow(tlr_info), 2)

  expect_equal(tlr_info$component, rep("numeric_range", 2))
  expect_true(all(tlr_info$source == "tailor"))
  expect_equal(tlr_info$name, c("lower_limit", "upper_limit"))
  expect_equal(tlr_info$id, c("lower_limit", "upper_limit"))

  expect_equal(tlr_info$object[[1]], dials::lower_limit(c(-Inf, Inf)))
  expect_equal(tlr_info$object[[2]], dials::upper_limit(c(-Inf, Inf)))
})

# -------------------------------------------------------------------------

test_that("extract single parameter from tailor with no adjustments", {
  skip_if_not_installed("dials")

  expect_snapshot(
    error = TRUE,
    extract_parameter_dials(tailor(), parameter = "none there")
  )
})

test_that("extract single parameter from tailor with no tunable parameters", {
  skip_if_not_installed("dials")

  tlr <-
    tailor() %>%
    adjust_numeric_calibration()

  expect_snapshot(
    error = TRUE,
    extract_parameter_dials(tlr, parameter = "none there")
  )
})

test_that("extract single parameter from tailor with tunable parameters", {
  skip_if_not_installed("dials")

  tlr <-
    tailor() %>%
    adjust_numeric_calibration() %>%
    adjust_numeric_range(
      lower_limit = hardhat::tune(),
      upper_limit = hardhat::tune()
    )

  expect_equal(
    extract_parameter_dials(tlr, "lower_limit"),
    dials::lower_limit()
  )
})
