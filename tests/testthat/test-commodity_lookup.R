context("commodity_lookup")

# All tests on the expected return data.
test_that("lookup return values are correct, and fail when expected", {
  skip_on_cran()

  df <- ct_commodities_table(type = "HS")
  ex_char <- commodity_lookup(value = "halibut", lookuptable = df)
  ex_num <- commodity_lookup(value = 1602, lookuptable = df)

  # Correct return data type for ex_char.
  expect_is(ex_char, "character")

  # Number of return values for ex_char.
  expect_equal(length(ex_char), 4)

  # Correct return data type for ex_num.
  expect_is(ex_num, "character")

  # Number of return values for ex_num.
  expect_equal(length(ex_num), 11)

  # Correct return values when input for "value" not found in lookup table.
  expect_equal(commodity_lookup(value = "not_a_trade_item",
                                lookuptable = df),
               "No matching results found")

  # Throw error with invalid input for param "value".
  expect_error(commodity_lookup(value = c("shrimp", "halibut"),
                                lookuptable = df))

  # Throw error with invalid input for param "lookuptable".
  expect_error(commodity_lookup(value = "halibut",
                                lookuptable = "lookuptable"))
})
