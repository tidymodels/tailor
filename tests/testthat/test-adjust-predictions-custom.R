test_that("adjustment printing", {
  expect_snapshot(container() %>% adjust_predictions_custom())
})
