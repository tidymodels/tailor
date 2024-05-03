test_that("adjustment printing", {
  expect_snapshot(tailor() %>% adjust_equivocal_zone())
  expect_snapshot(tailor() %>% adjust_equivocal_zone(hardhat::tune()))
})
