test_that("ct_commodity_lookup returns correct values for commodity codes", {
  results <- ct_commodity_lookup(c("020820"), return_code = TRUE)
  expect_equal(results, list("020820" = "020820"))
})

test_that("ct_commodity_lookup returns correct values for commodity names", {
  results <- ct_commodity_lookup(c("Frog legs"), return_code = FALSE)
  expect_equal(results, list("Frog legs" = "020820 - Frog legs, fresh, chilled or frozen"))
})

test_that("ct_commodity_lookup returns correct values when return_char = TRUE", {
  results <- ct_commodity_lookup(c("frog legs"), return_code = FALSE, return_char = TRUE)
  expect_equal(results, c("020820 - Frog legs, fresh, chilled or frozen"))
})

test_that("ct_commodity_lookup warns when no matching results are found", {
  expect_warning(ct_commodity_lookup(c("Nonexistent"), return_code = FALSE, verbose = TRUE))
})

test_that("ct_commodity_lookup returns empty result for non-matching search term", {
  results <- ct_commodity_lookup(c("Nonexistent"), return_code = FALSE, verbose = FALSE)
  expect_equal(results, list("Nonexistent" = character(0)))
})
