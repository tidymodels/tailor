test_that("check_container raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(container() %>% adjust_probability_threshold(.5))
})
