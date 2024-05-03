test_that("tailor printing", {
  expect_snapshot(tailor())
  expect_snapshot(tailor(type = "binary"))
  expect_snapshot(
    tailor(type = "binary") %>%
      adjust_probability_threshold(.2)
  )
  expect_snapshot(
    tailor(type = "binary") %>%
      adjust_probability_threshold(.2) %>%
      adjust_equivocal_zone()
  )
})
