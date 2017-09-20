context("ct_search")

test_that("correct api vals given: 1 reporter, 1 partner, imports, monthly", {
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


msg <- paste0("correct api vals given: 1 reporter, 2 partners, exports, ",
              "annual, only shrimp")
test_that(msg, {
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
                    start_date = "all",
                    end_date = "all",
                    commod_codes = shrimp_codes)

  # Data type.
  expect_is(vals, "data.frame")

  # Number of variables.
  expect_equal(ncol(vals), 35)

  # Variable "Reporter".
  expect_equal(unique(vals$reporter), "USA")

  # Variable "Partner".
  expect_equal(sort(unique(vals$partner)), c("Germany", "Thailand"))

  # Variable "Trade Flow".
  expect_equal(unique(vals$trade_flow), "Export")

  # Variable "Period".
  expect_equal(sort(unique(vals$period))[1:3], c(1991, 1992, 1993))

  # Variable "Commodity Code".
  expect_equal(sort(unique(vals$commodity_code)), shrimp_codes)

  # Variable "Netweight (kg)".
  expect_is(vals$netweight_kg, "integer")
})


test_that("errors and warnings are thrown as expected", {
  # Throw error with invalid input for arg "reporters".
  expect_error(ct_search(reporters = "invalid_reporter",
                         partners = "Germany",
                         trade_direction = "imports"))

  # Throw error with invalid input for arg "partners".
  expect_error(ct_search(reporters = "Canada",
                         partners = "invalid_partner",
                         trade_direction = "imports"))

  # Throw error with invalid input for arg "trade_direction".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "invalid_td"))

  # Throw error with invalid input for arg "type".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         type = "invalid_type"))

  # Throw error with invalid input for arg "freq".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         freq = "invalid_freq"))

  # Throw error with invalid input for arg "start_date" and "end_date".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         freq = "monthly",
                         start_date = "1/1/2011",
                         end_date = "5/1/2011"))

  # Returned error msg from the API with invalid input for arg "commod_codes".
  expect_error(ct_search(reporters = "Canada",
                    partners = "Germany",
                    trade_direction = "imports",
                    commod_codes = "invalid_codes"))

  # Throw error with invalid input for arg "col_name".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         trade_direction = "imports",
                         col_name = "invalid_fmt"))
})
