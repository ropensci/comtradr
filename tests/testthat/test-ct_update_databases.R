context("ct_update_databases")


test_that("func produces the correct message", {
  skip_on_cran()

  expect_message(
    ct_update_databases(force = TRUE),
    regexp = paste("Updates found. The following datasets have",
                   "been downloaded: commodities DB, reporter",
                   "countries, partner countries"),
    fixed = TRUE
  )
})


test_that("throw error with invalid input to arg 'force'", {
  expect_error(ct_update_databases(force = "invalid_input"))
})


test_that("throw error with invalid input to arg 'verbose'", {
  expect_error(ct_update_databases(verbose = "invalid_input"))
})


test_that("throw error with invalid input to arg 'commodity_type'", {
  expect_error(ct_update_databases(commodity_type = "invalid_input"))
})


test_that("throw curl error with invalid url str to arg 'commodity_url'", {
  skip_on_cran()

  expect_error(
    ct_update_databases(
      commodity_url = "not_a_url",
      regexp = "curl",
      fixed = TRUE
    )
  )
})
