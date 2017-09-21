context("ct_commodity_lookup")


test_that("correct vals given return_code == FALSE & return_char == TRUE", {
  vals <- ct_commodity_lookup(search_terms = "halibut",
                              return_code = FALSE,
                              return_char = TRUE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "character")

  # Number of return values.
  expect_equal(length(vals), 2)
})


test_that("correct vals given return_code == TRUE & return_char == FALSE", {
  vals <- ct_commodity_lookup(search_terms = 1602,
                              return_code = TRUE,
                              return_char = FALSE,
                              verbose = TRUE)

  # Correct return data type.
  expect_is(vals, "list")

  # Number of return values.
  expect_equal(length(vals), 1)
})


test_that("correct vals given return_code == TRUE & return_char == TRUE", {
  vals <- ct_commodity_lookup(search_terms = c(010119, 010231, 010594),
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
  vals <- ct_commodity_lookup(search_terms = c("tomato", "trout"),
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

test_that("throw warning with invalid input to arg 'search_terms'", {
  expect_warning(ex_5 <- ct_commodity_lookup(search_terms = "not_a_trade_item",
                                             return_char = TRUE))
})

test_that("throw error with invalid input to arg 'value'", {
  expect_error(ct_commodity_lookup(search_terms = list()))
})
