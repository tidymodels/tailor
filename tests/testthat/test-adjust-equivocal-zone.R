test_that("adjustment printing", {
  expect_snapshot(container() %>% adjust_equivocal_zone())
  expect_snapshot(container() %>% adjust_equivocal_zone(hardhat::tune()))
})
