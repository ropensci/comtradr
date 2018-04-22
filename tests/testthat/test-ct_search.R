context("ct_search")


test_that("correct api vals given: 1 reporter, 1 partner, imports, monthly", {
  skip_on_cran()

  # Get monhtly data on all German imports into Canada,
  # 2011-01-01 thru 2011-05-01.
  vals <- ct_search(reporters = "Canada",
                    partners = "Germany",
                    trade_direction = "imports",
                    freq = "monthly",
                    start_date = "2011-01",
                    end_date = "2011-05")

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
  expect_equal(sort(vals$period)[1], 201101)

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
  expect_equal(sort(unique(vals$year))[1:3], c(2011, 2012, 2013))

  # Variable "Commodity Code".
  expect_equal(sort(unique(vals$commodity_code)), shrimp_codes)

  # Variable "Netweight (kg)".
  expect_is(vals$netweight_kg, "integer")
})


test_that("correct api vals given: type == 'services'", {
  vals <- ct_search(reporters = "USA",
                    partners = "World",
                    freq = "annual",
                    start_date = "2015-01-01",
                    end_date = "2015-12-31",
                    commod_codes = "TOTAL",
                    type = "services")
  expect_true(nrow(vals) > 0)
})


test_that("throw error with invalid input to arg 'reporters'", {
  expect_error(ct_search(reporters = "invalid_reporter",
                         partners = "Germany",
                         trade_direction = "imports"))
})


test_that("throw error with invalid input to arg 'partners'", {
  expect_error(ct_search(reporters = "all",
                         partners = "invalid_partner",
                         trade_direction = "imports",
                         start_date = "2011-01-01",
                         end_date = "2012-01-01"))
})


test_that("throw error with invalid input to arg 'trade_direction'", {
  expect_error(ct_search(reporters = "Canada",
                         partners = "all",
                         trade_direction = "invalid_td",
                         start_date = "2011-01-01",
                         end_date = "2012-01-01"))
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


test_that("throw error with invalid input to arg 'commod_codes'", {
  skip_on_cran()

  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "all",
                         commod_codes = "invalid_codes"),
               regexp = "invalid_codes is an invalid commodity code.",
               fixed = TRUE)
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


test_that("throw error with more than 20 commodity codes", {
  expect_error(
    ct_search(reporters = "Germany",
              partners = "Japan",
              trade_direction = c("imports",
                                  "exports",
                                  "re_imports",
                                  "re_exports"),
              commod_codes = ct_commodity_lookup("frozen",
                                                 return_char = TRUE,
                                                 return_code = TRUE))
  )
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


test_that("throw error with invalid input to arg 'start_date' & 'end_date'", {
  expect_error(ct_search(reporters = "Canada",
               partners = "Germany",
               trade_direction = "imports",
               freq = "monthly",
               start_date = "1/1/2011",
               end_date = "5/1/2011"),
  regexp = "arg 'start_date' must be a date with one of these formats:")
})


test_that("different date inputs produce correct date ranges", {
  # Tests with "freq" is "annual".
  expect_equal(get_date_range("2016", "2016", "A"), "2016")
  expect_equal(get_date_range("2016-01-01", 2016, "A"), "2016")
  expect_equal(get_date_range("2013", "2016", "A"),
               "2013%2C2014%2C2015%2C2016")
  expect_equal(get_date_range(2010, 2012, "A"), "2010%2C2011%2C2012")
  expect_error(
    get_date_range("2008", "2016", "A"),
    regexp = "cannot search more than five consecutive years/months"
  )

  # Tests with "freq" as "monthly".
  expect_equal(get_date_range("2016-01-01", "2016-05", "M"),
               "201601%2C201602%2C201603%2C201604%2C201605")
  expect_equal(get_date_range(2016, 2016, "M"), "2016")
  expect_error(get_date_range("2013", "2016", "M"),
               regexp = "Cannot get more than a single year's worth")
  expect_error(
    get_date_range(2015, "2015-03", "M"),
    rexexp = "'start_date' and 'end_date' must have the same format"
  )
})
