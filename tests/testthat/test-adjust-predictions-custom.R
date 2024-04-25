test_that("adjustment printing", {
  expect_snapshot(ctr_cls %>% adjust_predictions_custom())
})
