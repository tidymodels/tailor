test_that("basic adjust_probability_threshold() usage works", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  # fitting and predicting happens without raising conditions
  expect_no_condition(
    tlr <-
      tailor() %>%
      adjust_probability_threshold(.1)
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

  # column names are as expected
  expect_equal(colnames(two_class_example), colnames(tlr_pred))

  # calculations match those done manually
  manual_pred <- factor(ifelse(two_class_example$Class1 > .1, "Class1", "Class2"))
  expect_equal(tlr_pred$predicted, manual_pred)
})

test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_probability_threshold())
  expect_snapshot(tailor() %>% adjust_probability_threshold(hardhat::tune()))
})
