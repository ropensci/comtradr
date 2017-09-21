context("ct_search")


test_that("correct api vals given: 1 reporter, 1 partner, imports, monthly", {
  skip_on_cran()

  # Get monhtly data on all German imports into Canada,
  # 2011-01-01 thru 2011-05-01.
  vals <- ct_search(reporters = "Canada",
                    partners = "Germany",
                    trade_direction = "imports",
                    freq = "monthly",
                    start_date = "2011-01-01",
                    end_date = "2011-05-01")

  # Data type.
  expect_is(vals, "data.frame")

  # Number of variables.
  expect_equal(ncol(vals), 35)

  # Variable "Reporter".
  expect_equal(unique(vals$reporter), "Canada")

  # Variable "Partner".
  expect_equal(unique(vals$partner), "Germany")

  # Variable "Trade Flow".
  expect_equal(unique(vals$trade_flow), "Imports")

  # Variable "Period".
  expect_equal(vals$period[1], 201101)

  # Variable "Commodity Code".
  expect_equal(unique(vals$commodity_code), "TOTAL")
})


msg <- paste0("correct api vals given: 1 reporter, 2 partners, ",
              "all trade directions, annual, only shrimp")
test_that(msg, {
  skip_on_cran()

  # Get yearly data on US shrimp exports into Germany and Thailand,
  # for all years on record.
  shrimp_codes <- c("030613",
                    "030623",
                    "160520",
                    "160521",
                    "160529")
  vals <- ct_search(reporters = "USA",
                    partners = c("Germany", "Thailand"),
                    trade_direction = "exports",
                    freq = "annual",
                    start_date = "2011-01-01",
                    end_date = "2015-01-01",
                    commod_codes = shrimp_codes)

  # Data type.
  expect_is(vals, "data.frame")

  # Number of variables.
  expect_equal(ncol(vals), 35)

  # Variable "Reporter".
  expect_equal(unique(vals$reporter), "USA")

  # Variable "Partner".
  expect_equal(sort(unique(vals$partner)), c("Germany", "Thailand"))

  # Variable "Period".
  expect_equal(sort(unique(vals$period))[1:3], c(2011, 2012, 2013))

  # Variable "Commodity Code".
  expect_equal(sort(unique(vals$commodity_code)), shrimp_codes)

  # Variable "Netweight (kg)".
  expect_is(vals$netweight_kg, "integer")
})


test_that("throw error with invalid input to arg 'reporters'", {
  expect_error(ct_search(reporters = "invalid_reporter",
                         partners = "Germany",
                         trade_direction = "imports"))
})


test_that("throw error with invalid input to arg 'partners'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "invalid_partner",
                         trade_direction = "imports"))
})


test_that("throw error with invalid input to arg 'trade_direction'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "invalid_td"))
})


test_that("throw error with invalid input to arg 'type'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         type = "invalid_type"))
})


test_that("throw error with invalid input to arg 'freq'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         freq = "invalid_freq"))
})


test_that("throw error with invalid input to arg 'start_date' & 'end_date'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         freq = "monthly",
                         start_date = "1/1/2011",
                         end_date = "5/1/2011"))
})


test_that("throw error with invalid input to arg 'commod_codes'", {
  skip_on_cran()

  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         commod_codes = "invalid_codes"))
})


test_that("throw error with invalid input to arg 'col_name'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         col_name = "invalid_fmt"))
})


test_that("throw error with too many catch-all 'all' input values", {
  expect_error(ct_search(reporters = "all",
                         partners = "all"))
})


test_that("throw error with more than five specified reporter countries", {
  expect_error(ct_search(reporters = c("Canada", "USA", "Mexico", "Germany",
                                       "France", "China"),
                         partners = "Japan"))
})


test_that("throw error when hourly query limit is at zero", {
  # Get current rate limit values.
  cache_vals <- get_cache_values()

  # Assign the hourly limit value to be 0.
  assign("queries_this_hour", 0, envir = ct_env)

  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany"))

  # Set the rate limit value back to what it was previously.
  assign("queries_this_hour", cache_vals$queries_this_hour, envir = ct_env)
})
