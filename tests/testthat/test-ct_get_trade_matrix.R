# Unit tests for the internal commodity-code validator (no network) --------
test_that("check_matrix_cmdCode validates and normalises commodity codes", {
  expect_equal(comtradr:::check_matrix_cmdCode("TOTAL"), "TOTAL")
  expect_equal(comtradr:::check_matrix_cmdCode(c("0", "1")), "0,1")
  expect_equal(comtradr:::check_matrix_cmdCode("everything"), NULL)
  # whitespace is squished
  expect_equal(comtradr:::check_matrix_cmdCode(" 0 "), "0")

  expect_error(comtradr:::check_matrix_cmdCode("27"),
               "commodity codes you provided are invalid")
  expect_error(comtradr:::check_matrix_cmdCode("HS"),
               "commodity codes you provided are invalid")
})


# URL construction tests (no network) -------------------------------------
without_internet({
  test_that("URL query is correctly constructed for the trade matrix", {
    expect_GET(
      comtradr::ct_get_trade_matrix(
        commodity_code = "0",
        flow_direction = "export",
        reporter = "ARG",
        partner = "World",
        start_date = 2023,
        end_date = 2023,
        primary_token = "test"
      ),
      "https://comtradeapi.un.org/data/v1/getTradeMatrix/C/A/TM?cmdCode=0&flowCode=X&partnerCode=0&reporterCode=32&period=2023&includeDesc=TRUE") # nolint
  })
})


# Replay test using a recorded fixture ------------------------------------
httptest2::with_mock_dir("trade_matrix", simplify = FALSE, {
  test_that("We can get trade matrix data", {
    expect_s3_class(
      comtradr::ct_get_trade_matrix(
        commodity_code = "TOTAL",
        flow_direction = "export",
        reporter = "DEU",
        partner = "FRA",
        start_date = 2023,
        end_date = 2023,
        primary_token = "test"
      ),
      "data.frame")
  })
})


# Date validation: frequency is hardcoded annual --------------------------
test_that("monthly-style dates are coerced to the year under annual frequency", {
  # The trade matrix endpoint hardcodes frequency = "A". check_date() does not
  # reject a monthly-style "yyyy-mm" input; it coerces it to the containing
  # year rather than raising an error. Document that actual behaviour here.
  expect_equal(
    comtradr:::check_date("2023-01", "2023-01", frequency = "A", bulk = FALSE),
    "2023"
  )
})
