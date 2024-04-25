test_that("adjustment printing", {
  expect_snapshot(ctr_cls %>% adjust_probability_threshold())
  expect_snapshot(ctr_cls %>% adjust_probability_threshold(hardhat::tune()))
})
