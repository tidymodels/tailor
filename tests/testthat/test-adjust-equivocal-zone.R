skip_if_not_installed("probably")

test_that("basic adjust_equivocal_zone() usage works", {
  skip_if_not_installed("modeldata")
  library(dplyr)
  library(modeldata)

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() %>%
      adjust_equivocal_zone(value = 1 / 4)
  )

  expect_no_condition(
    tlr_fit <- fit(
      tlr,
      two_class_example,
      outcome = c(truth),
      estimate = c(predicted),
      probabilities = c(Class1, Class2)
    )
  )

  expect_no_condition(
    tlr_pred <- predict(tlr_fit, two_class_example)
  )

  # classes are as expected
  expect_s3_class(tlr, "tailor")
  expect_s3_class(tlr_fit, "tailor")
  expect_s3_class(tlr_pred, "tbl_df")
  expect_s3_class(tlr_pred$predicted, "class_pred")

  # column names are as expected
  expect_equal(colnames(two_class_example), colnames(tlr_pred))

  # prediction classes are as expected
  expect_equal(levels(tlr_pred$predicted), levels(two_class_example$predicted))
  expect_equal(attr(tlr_pred$predicted, "equivocal"), "[EQ]")

  # calculations match those done manually
  tlr_pred_col <- as.character(tlr_pred$predicted)
  manual_pred_col <- ifelse(two_class_example$Class1 > .5, "Class1", "Class2")
  manual_pred_col <-
    ifelse(
      two_class_example$Class1 > .25 & two_class_example$Class1 < .75,
      NA_character_,
      manual_pred_col
    )
  expect_equal(tlr_pred_col, manual_pred_col)
})

# TODO: test sensitivity to function arguments

test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_equivocal_zone())
  expect_snapshot(tailor() %>% adjust_equivocal_zone(hardhat::tune()))
})

test_that("tunable", {
  tlr <-
    tailor() %>%
    adjust_equivocal_zone(value = 1 / 4)
  adj_param <- tunable(tlr$adjustments[[1]])
  expect_equal(adj_param$name, c("buffer"))
  expect_true(all(adj_param$source == "tailor"))
  expect_true(is.list(adj_param$call_info))
  expect_equal(nrow(adj_param), 1)
  expect_equal(
    names(adj_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})
