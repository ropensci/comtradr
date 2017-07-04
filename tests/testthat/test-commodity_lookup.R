context("commodity_lookup")

# All tests on the expected return data.
test_that("lookup return values are correct, and fail when expected", {
  skip_on_cran()
  skip_on_travis()

  df <- ct_commodities_table(type = "HS")
  ex_1 <- commodity_lookup(values = "halibut",
                           lookuptable = df,
                           return_code = FALSE,
                           return_char = TRUE,
                           verbose = TRUE)
  ex_2 <- commodity_lookup(values = 1602,
                           lookuptable = df,
                           return_code = TRUE,
                           return_char = FALSE,
                           verbose = TRUE)
  ex_3 <- commodity_lookup(values = df$commodity[c(10, 20, 40)],
                           lookuptable = df,
                           return_code = TRUE,
                           return_char = TRUE,
                           verbose = TRUE)

  # Correct return data type.
  expect_is(ex_1, "character")
  expect_is(ex_2, "list")
  expect_is(ex_3, "character")

  # Number of return values.
  expect_equal(length(ex_1), 2)
  expect_equal(length(ex_2), 1)
  expect_equal(length(ex_3), 3)

  # Make sure example 3 is not a named vector.
  expect_named(ex_3, NULL)

  # Correct return values when input for "value" not found in lookup table.
  expect_warning(ex_4 <- commodity_lookup(values = "not_a_trade_item",
                                          lookuptable = df,
                                          return_char = TRUE))
  expect_equal(length(ex_4), 0)

  # Throw error with invalid input for param "value".
  expect_error(commodity_lookup(value = list(), lookuptable = df))

  # Throw error with invalid input for param "lookuptable".
  expect_error(commodity_lookup(values = "halibut",
                                lookuptable = "lookuptable"))
})
