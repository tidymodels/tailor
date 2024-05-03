test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_predictions_custom())
})
