context("ct_search")

# All tests on the expected return data.
test_that("search return values are correct, and fail when expected", {
  #skip_on_cran()
  #skip_on_travis()

  countrydf <- ct_countries_table()

  # Get monhtly data on all German imports into Canada,
  # 2011-01-01 thru 2011-05-01.
  ex1 <- ct_search(reporters = "Canada",
                   partners = "Germany",
                   countrytable = countrydf,
                   tradedirection = "imports",
                   freq = "monthly",
                   startdate = "2011-01-01",
                   enddate = "2011-05-01")
  # Get yearly data on Canadian shrimp exports into Germany and Thailand,
  # for all years on record.
  shrimp_codes <- c("030613",
                    "030623",
                    "160520",
                    "160521",
                    "160529")
  ex2 <- ct_search(reporters = "USA",
                   partners = c("Germany", "Thailand"),
                   countrytable = countrydf,
                   tradedirection = "exports",
                   freq = "annual",
                   startdate = "all",
                   enddate = "all",
                   commodcodes = shrimp_codes)

  ## ex1 tests
  # Data type.
  expect_is(ex1, "list")
  expect_is(ex1$data, "data.frame")

  # Number of variables.
  expect_equal(ncol(ex1$data), 35)

  # Variable "Reporter".
  expect_equal(unique(ex1$data$Reporter), "Canada")

  # Variable "Partner".
  expect_equal(unique(ex1$data$Partner), "Germany")

  # Variable "Trade Flow".
  expect_equal(unique(ex1$data$`Trade Flow`), "Imports")

  # Variable "Period".
  expect_equal(ex1$data$Period[1], 201101)

  # Variable "Commodity Code".
  expect_equal(unique(ex1$data$`Commodity Code`), "TOTAL")

  ## ex2 tests
  # Data type.
  expect_is(ex2, "list")
  expect_is(ex2$data, "data.frame")

  # Number of variables.
  expect_equal(ncol(ex2$data), 35)

  # Variable "Reporter".
  expect_equal(unique(ex2$data$Reporter), "USA")

  # Variable "Partner".
  expect_equal(sort(unique(ex2$data$Partner)), c("Germany", "Thailand"))

  # Variable "Trade Flow".
  expect_equal(unique(ex2$data$`Trade Flow`), "Export")

  # Variable "Period".
  expect_equal(sort(unique(ex2$data$Period))[1:3], c(1991, 1992, 1993))

  # Variable "Commodity Code".
  expect_equal(sort(unique(ex2$data$`Commodity Code`)), shrimp_codes)

  # Variable "Netweight (kg)".
  expect_is(ex2$data$`Netweight (kg)`, "integer")

  ## Check that ct_search is failing as expected.
  # Throw error with invalid input for param "reporters".
  expect_error(ct_search(reporters = "invalid_reporter",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports"))
  # Throw error with invalid input for param "partners".
  expect_error(ct_search(reporters = "Canada",
                         partners = "invalid_partner",
                         countrytable = countrydf,
                         tradedirection = "imports"))
  # Throw error with invalid input for param "countrytable".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = data.frame(),
                         tradedirection = "imports"))
  # Throw error with invalid input for param "tradedirection".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "invalid_td"))
  # Throw error with invalid input for param "type".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         type = "invalid_type"))
  # Throw error with invalid input for param "freq".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         freq = "invalid_freq"))
  # Throw error with invalid input for params "startdate" and "endate".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         freq = "monthly",
                         startdate = "1/1/2011",
                         enddate = "5/1/2011"))
  # Returned error msg from the API with invalid input for param "commodcodes".
  ex1 <- ct_search(reporters = "Canada",
                   partners = "Germany",
                   countrytable = countrydf,
                   tradedirection = "imports",
                   commodcodes = "invalid_codes")
  expect_equal(ex1$details, "invalid_codes is an invalid commodity code.")
  # Throw error with invalid input for param "fmt".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         fmt = "invalid_fmt"))
  # Throw error with invalid input for param "colname".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         colname = "invalid_fmt"))
  # Throw error with invalid input for param "codetype".
  expect_error(ct_search(reporters = "Canada",
                         partners = "Germany",
                         countrytable = countrydf,
                         tradedirection = "imports",
                         codetype = "invalid_codetype"))
})
