test_that("check_tailor raises informative error", {
  expect_snapshot(error = TRUE, adjust_probability_threshold("boop"))
  expect_no_condition(tailor() %>% adjust_probability_threshold(.5))
})
