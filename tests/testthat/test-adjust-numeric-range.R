test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_numeric_range())
  expect_snapshot(tailor() %>% adjust_numeric_range(hardhat::tune()))
  expect_snapshot(tailor() %>% adjust_numeric_range(-1, hardhat::tune()))
  expect_snapshot(tailor() %>% adjust_numeric_range(hardhat::tune(), 1))
})

