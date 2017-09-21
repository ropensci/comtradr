context("ct_commodity_db_type")

test_that("correct return value", {
  expect_equal(ct_commodity_db_type(), "HS")
})
