test_that("adjustment printing", {
  expect_snapshot(ctr_cls %>% adjust_equivocal_zone())
  expect_snapshot(ctr_cls %>% adjust_equivocal_zone(hardhat::tune()))
})
