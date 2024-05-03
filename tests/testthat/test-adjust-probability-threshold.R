test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_probability_threshold())
  expect_snapshot(tailor() %>% adjust_probability_threshold(hardhat::tune()))
})
