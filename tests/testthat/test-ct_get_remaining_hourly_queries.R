context("ct_get_remaining_hourly_queries")

test_that("func returns the correct int value", {
  expect_equal(ct_get_remaining_hourly_queries(), 100)
})
