context("ct_use_pretty_cols")


test_that("func properly converts snake_case col headers", {
  # Create empty df with snake_case API col headers.
  df <- matrix(ncol = 35, nrow = 0) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(names(cols))

  # Replace snake_case col headers with polished headers.
  df <- ct_use_pretty_cols(df)

  # Check values of output.
  expect_equal(colnames(df), names(comtradr::ct_pretty_cols))
})


test_that("input df with polished headers returns identical df", {
  # Create empty df with polished API col headers.
  df <- matrix(ncol = 35, nrow = 0) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(names(cols)) %>%
    ct_use_pretty_cols

  # Check values of output.
  expect_equal(df, ct_use_pretty_cols(df))
})
