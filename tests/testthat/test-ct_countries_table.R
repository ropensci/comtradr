context("ct_countries_table")

# All tests on the expected return data.
test_that("return has correct attributes, and fails when expected", {
  skip_on_cran()
  skip_on_travis()

  df <- ct_countries_table()

  # Data type.
  expect_is(df, "data.frame")

  # Number of observations.
  expect_equal(nrow(df), 548)

  # Col names and number of variables.
  expect_equal(colnames(df), c("code", "country name", "type"))

  # Number of unique countries.
  expect_equal(length(unique(df$code)), 294)

  # Correct values seen in col "type".
  expect_equal(unique(df$type), c("reporter", "partner"))
})
