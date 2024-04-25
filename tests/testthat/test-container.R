test_that("container printing", {
  expect_snapshot(ctr_cls)
  expect_snapshot(container(mode = "classification", type = "binary"))
  expect_snapshot(
    container(mode = "classification", type = "binary") %>%
      adjust_probability_threshold(.2)
  )
  expect_snapshot(
    container(mode = "classification", type = "binary") %>%
      adjust_probability_threshold(.2) %>%
      adjust_equivocal_zone()
  )
})
