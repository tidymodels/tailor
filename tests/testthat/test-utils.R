test_that("check_container raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(ctr_cls %>% adjust_probability_threshold(.5))
})
