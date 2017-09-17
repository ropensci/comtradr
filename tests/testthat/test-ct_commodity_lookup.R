context("ct_commodity_lookup")


test_that("correct vals given return_code == FALSE & return_char == TRUE", {
  vals <- ct_commodity_lookup(values = "halibut",
                              return_code = FALSE,
                              return_char = TRUE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "character")

  # Number of return values.
  expect_equal(length(vals), 2)
})


test_that("correct vals given return_code == TRUE & return_char == FALSE", {
  vals <- ct_commodity_lookup(values = 1602,
                              return_code = TRUE,
                              return_char = FALSE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "list")

  # Number of return values.
  expect_equal(length(vals), 1)
})


test_that("correct vals given return_code == TRUE & return_char == TRUE", {
  vals <- ct_commodity_lookup(values = c(010119, 010231, 010594),
                              return_code = TRUE,
                              return_char = TRUE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "character")

  # Number of return values.
  expect_equal(length(vals), 6)

  # Make sure vals is not a named vector.
  expect_named(vals, NULL)
})


test_that("correct vals given return_code == FALSE & return_char == FALSE", {
  vals <- ct_commodity_lookup(values = c("tomato", "trout"),
                              return_code = FALSE,
                              return_char = FALSE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "list")

  # Number of return values.
  expect_equal(length(vals), 2)

  # Make sure vals is a named list..
  expect_named(vals, c("tomato", "trout"))
})


test_that("errors and warnings are thrown as expected", {
  # Correct return values when input for "value" not found in lookup table.
  expect_warning(ex_5 <- ct_commodity_lookup(values = "not_a_trade_item",
                                             return_char = TRUE))
  expect_equal(length(ex_5), 0)

  # Throw error with invalid input for param "value".
  expect_error(ct_commodity_lookup(value = list()))
})
