context("ct_get_reset_time")

test_that("same test, different iteration", {
  expect_is(ct_get_reset_time(), "POSIXct")
})
