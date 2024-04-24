test_that("container printing", {
  expect_snapshot(container())
  expect_snapshot(container(type = "binary"))
  expect_snapshot(
    container(type = "binary") %>% adjust_probability_threshold(.2)
  )
  expect_snapshot(
    container(type = "binary") %>%
      adjust_probability_threshold(.2) %>%
      adjust_equivocal_zone()
  )
})
