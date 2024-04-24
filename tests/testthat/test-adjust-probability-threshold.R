test_that("adjustment printing", {
  expect_snapshot(container() %>% adjust_probability_threshold())
  expect_snapshot(container() %>% adjust_probability_threshold(hardhat::tune()))
})
