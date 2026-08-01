# Unit tests for the internal commodity-code validator (no network) --------
test_that("check_matrix_cmdCode validates and normalises commodity codes", {
  expect_equal(comtradr:::check_matrix_cmdCode("TOTAL"), "TOTAL")
  expect_equal(comtradr:::check_matrix_cmdCode(c("0", "1")), "0,1")
  # whitespace is squished
  expect_equal(comtradr:::check_matrix_cmdCode(" 0 "), "0")

  # `everything` means all ten one-digit sections, which is the `ag1` level
  # selector -- NOT omitting cmdCode, which returns the whole nested hierarchy.
  expect_equal(comtradr:::check_matrix_cmdCode("everything"), "ag1")

  # Two- and three-digit SITC codes are served by the endpoint. The UN's own
  # reference example uses "01", so these must not be rejected.
  expect_equal(comtradr:::check_matrix_cmdCode("01"), "01")
  expect_equal(comtradr:::check_matrix_cmdCode("011"), "011")
  expect_equal(comtradr:::check_matrix_cmdCode(c("TOTAL", "01")), "TOTAL,01")

  # The five four-/five-digit codes the UN estimates in addition.
  expect_equal(comtradr:::check_matrix_cmdCode("7812"), "7812")
  expect_equal(comtradr:::check_matrix_cmdCode("78531"), "78531")

  # Level selectors, case-insensitive.
  expect_equal(comtradr:::check_matrix_cmdCode("ag1"), "ag1")
  expect_equal(comtradr:::check_matrix_cmdCode("AG3"), "ag3")

  # Codes the endpoint does not serve are still rejected -- the API answers
  # these with HTTP 200 and zero rows, so we have to catch them ourselves.
  expect_error(comtradr:::check_matrix_cmdCode("HS"),
               "commodity codes you provided are invalid")
  expect_error(comtradr:::check_matrix_cmdCode("ZZZ"),
               "commodity codes you provided are invalid")
  expect_error(comtradr:::check_matrix_cmdCode("0111"),
               "commodity codes you provided are invalid")
  expect_error(comtradr:::check_matrix_cmdCode("ag6"),
               "commodity codes you provided are invalid")

  # NA used to make all(valid) NA and abort with the base error
  # "missing value where TRUE/FALSE needed"
  expect_error(comtradr:::check_matrix_cmdCode(NA),
               "commodity codes you provided are invalid")
  expect_error(comtradr:::check_matrix_cmdCode(c("0", NA)),
               "commodity codes you provided are invalid")

  # an empty code would emit a bare `cmdCode=` in the URL
  expect_error(comtradr:::check_matrix_cmdCode(NULL), "must not be empty")
  expect_error(comtradr:::check_matrix_cmdCode(character(0)),
               "must not be empty")

  # numeric input is accepted, matching the rest of the package
  expect_equal(comtradr:::check_matrix_cmdCode(0), "0")
})

test_that("check_matrix_cmdCode warns that all_levels is nested", {
  expect_warning(
    result <- comtradr:::check_matrix_cmdCode("all_levels"),
    "nested"
  )
  # omitting cmdCode is what returns every level at once
  expect_null(result)
})


# Flow direction: the endpoint only carries imports and exports ------------
test_that("check_matrix_flowCode rejects flows the endpoint does not carry", {
  expect_equal(
    comtradr:::check_matrix_flowCode("export", update = FALSE,
                                     verbose = FALSE),
    "X"
  )
  # case-insensitive, consistent with ct_get_data()
  expect_equal(
    comtradr:::check_matrix_flowCode("Import", update = FALSE,
                                     verbose = FALSE),
    "M"
  )

  # These are valid entries in ct_get_ref_table('flow_direction'), but the
  # trade matrix returns an empty result for them, so we abort up front.
  expect_error(
    comtradr:::check_matrix_flowCode("re-export", update = FALSE,
                                     verbose = FALSE),
    "not available from the trade"
  )

  # `everything` requests both supported flows
  expect_null(
    comtradr:::check_matrix_flowCode("everything", update = FALSE,
                                     verbose = FALSE)
  )

  # a typo alongside `everything` must not be swallowed by the short circuit
  expect_error(
    comtradr:::check_matrix_flowCode(c("everything", "re-export"),
                                     update = FALSE, verbose = FALSE),
    "not available from the trade"
  )

  expect_error(
    comtradr:::check_matrix_flowCode(NA, update = FALSE, verbose = FALSE),
    "not available from the trade"
  )
})


# Date validation: annual only, sane ordering ------------------------------
test_that("check_matrix_dates accepts plain years only", {
  expect_null(comtradr:::check_matrix_dates(2022, 2023))
  expect_null(comtradr:::check_matrix_dates("2022", "2023"))
  # a missing date is caught downstream by check_date(), not here
  expect_null(comtradr:::check_matrix_dates(NULL, NULL))

  expect_error(comtradr:::check_matrix_dates("2022-01", 2023),
               "only provides annual data")

  # A Date carries a day and month, so it cannot be a plain year. This used to
  # iterate the underlying numeric and report "Invalid date 19358".
  expect_error(
    comtradr:::check_matrix_dates(as.Date("2023-01-01"), 2023),
    "only provides annual data"
  )
  expect_error(
    comtradr:::check_matrix_dates(as.Date("2023-01-01"), 2023),
    "2023-01-01"
  )

  # Reversed range used to fall through to seq.Date() and produce the base
  # error "wrong sign in 'by' argument".
  expect_error(comtradr:::check_matrix_dates(2023, 2020),
               "is after")
})


# End-to-end filtering with a stubbed request layer -----------------------
# These exercise the code AFTER a successful response, which the
# without_internet() URL tests cannot reach -- they abort at the mock.
fake_matrix <- function(tidy_cols = TRUE) {
  x <- data.frame(
    reporter_code = c(0, 0, 276, 276, 251),
    partner_code = c(0, 251, 0, 251, 276),
    primary_value = c(10, 4, 6, 1, 3)
  )
  if (!tidy_cols) {
    names(x)[1:2] <- c("reporterCode", "partnerCode")
  }
  attr(x, "url") <- "https://example.org"
  attr(x, "time") <- 0.5
  x
}

test_that("default reporter/partner 'everything' returns filtered data", {
  # Regression guard: reporter and partner both resolve to NULL on the
  # defaults, which previously reached strsplit(NULL, ",") and aborted with
  # "non-character argument" AFTER the request had already been made.
  testthat::local_mocked_bindings(
    ct_execute_request = function(...) fake_matrix()
  )

  result <- comtradr::ct_get_trade_matrix(
    start_date = 2022, end_date = 2022, primary_token = "test"
  )

  expect_s3_class(result, "data.frame")
  # only the two genuinely bilateral rows survive
  expect_equal(nrow(result), 2)
  expect_true(all(result$reporter_code != 0 & result$partner_code != 0))
  expect_equal(attr(result, "url"), "https://example.org")
})

test_that("include_world = TRUE keeps the margins untouched", {
  testthat::local_mocked_bindings(
    ct_execute_request = function(...) fake_matrix()
  )

  result <- comtradr::ct_get_trade_matrix(
    start_date = 2022, end_date = 2022,
    include_world = TRUE, primary_token = "test"
  )

  expect_equal(nrow(result), 5)
})

test_that("partner = 'World' keeps that margin but drops the reporter one", {
  # Regression guard: a blanket "user asked for World, keep everything"
  # escape hatch left the reporter margin and the grand total in place, so
  # the result still double counted.
  testthat::local_mocked_bindings(
    ct_execute_request = function(...) fake_matrix()
  )

  result <- comtradr::ct_get_trade_matrix(
    start_date = 2022, end_date = 2022,
    partner = "World", primary_token = "test"
  )

  # partner-World rows are what the user asked for and must survive ...
  expect_true(any(result$partner_code == 0))
  # ... but nothing with reporter World, including the grand total, which
  # would otherwise be counted a second time
  expect_false(any(result$reporter_code == 0))
  # both reporter-World rows of the fixture go, the other three stay
  expect_equal(nrow(result), 3)
})

test_that("process = FALSE skips filtering entirely", {
  response <- structure(list(), class = "httr2_response")
  testthat::local_mocked_bindings(
    ct_execute_request = function(...) response
  )

  expect_identical(
    comtradr::ct_get_trade_matrix(
      start_date = 2022, end_date = 2022,
      process = FALSE, primary_token = "test"
    ),
    response
  )
})


# World row filtering ------------------------------------------------------
test_that("drop_world_rows removes margins and keeps request metadata", {
  x <- data.frame(
    reporter_code = c(0, 0, 276, 276),
    partner_code = c(0, 251, 0, 251),
    primary_value = c(4, 3, 2, 1)
  )
  attr(x, "url") <- "https://example.org"
  attr(x, "time") <- 1.5

  result <- comtradr:::drop_world_rows(x, tidy_cols = TRUE)

  expect_equal(nrow(result), 1)
  expect_equal(result$primary_value, 1)
  # `[.data.frame` drops these, so they have to be put back explicitly
  expect_equal(attr(result, "url"), "https://example.org")
  expect_equal(attr(result, "time"), 1.5)

  # untidied column names
  y <- data.frame(
    reporterCode = c(0, 276),
    partnerCode = c(251, 251),
    primary_value = c(2, 1)
  )
  expect_equal(nrow(comtradr:::drop_world_rows(y, tidy_cols = FALSE)), 1)

  # an empty response is a placeholder without these columns; leave it alone
  empty <- data.frame(count = 0)
  expect_equal(comtradr:::drop_world_rows(empty, tidy_cols = TRUE), empty)

  # zero-row and non-data.frame inputs pass straight through
  expect_equal(nrow(comtradr:::drop_world_rows(x[0, ], tidy_cols = TRUE)), 0)

  # NA codes are kept rather than silently dropped
  na_codes <- data.frame(
    reporter_code = c(NA, 276), partner_code = c(251, 251),
    primary_value = c(2, 1)
  )
  expect_equal(nrow(comtradr:::drop_world_rows(na_codes, tidy_cols = TRUE)), 2)

  # factor and character code columns behave like numeric ones
  fct <- data.frame(
    reporter_code = factor(c("0", "276")),
    partner_code = factor(c("251", "251")),
    primary_value = c(2, 1)
  )
  expect_equal(nrow(comtradr:::drop_world_rows(fct, tidy_cols = TRUE)), 1)

  # a multi-chunk result carries a character *vector* url attribute
  chunked <- x
  attr(chunked, "url") <- c("https://a.example", "https://b.example")
  expect_length(
    attr(comtradr:::drop_world_rows(chunked, tidy_cols = TRUE), "url"), 2
  )

  # each dimension can be spared independently
  expect_equal(
    nrow(comtradr:::drop_world_rows(x, tidy_cols = TRUE,
                                    drop_reporter = FALSE)),
    2
  )
  expect_equal(
    nrow(comtradr:::drop_world_rows(x, tidy_cols = TRUE,
                                    drop_partner = FALSE)),
    2
  )
})

test_that("requests_world detects an explicit World request", {
  expect_false(comtradr:::requests_world(NULL))
  expect_false(comtradr:::requests_world(character(0)))
  expect_true(comtradr:::requests_world("0"))
  expect_true(comtradr:::requests_world("0,276"))
  # a code that merely contains a zero is not World
  expect_false(comtradr:::requests_world("10"))
  expect_false(comtradr:::requests_world("100,276"))
})

test_that("include_world must be a single logical", {
  expect_error(
    comtradr::ct_get_trade_matrix(
      start_date = 2022, end_date = 2022,
      include_world = "yes", primary_token = "test"
    ),
    "include_world"
  )
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

  test_that("`everything` requests one-digit sections via the ag1 selector", {
    expect_GET(
      comtradr::ct_get_trade_matrix(
        commodity_code = "everything",
        flow_direction = "export",
        reporter = "ARG",
        partner = "World",
        start_date = 2023,
        end_date = 2023,
        primary_token = "test"
      ),
      "https://comtradeapi.un.org/data/v1/getTradeMatrix/C/A/TM?cmdCode=ag1&flowCode=X&partnerCode=0&reporterCode=32&period=2023&includeDesc=TRUE") # nolint
  })

  test_that("the default query omits reporter and partner codes", {
    # reporter/partner "everything" resolve to NULL, i.e. the parameters are
    # left out of the URL entirely. This is the function's default shape.
    expect_GET(
      comtradr::ct_get_trade_matrix(
        start_date = 2022,
        end_date = 2022,
        primary_token = "test"
      ),
      "https://comtradeapi.un.org/data/v1/getTradeMatrix/C/A/TM?cmdCode=TOTAL&flowCode=M%2CX&period=2022&includeDesc=TRUE") # nolint
  })
})


# Replay test using a recorded fixture ------------------------------------
httptest2::with_mock_dir("trade_matrix", simplify = FALSE, {
  test_that("We can get trade matrix data", {
    result <- comtradr::ct_get_trade_matrix(
      commodity_code = "TOTAL",
      flow_direction = "export",
      reporter = "DEU",
      partner = "FRA",
      start_date = 2023,
      end_date = 2023,
      primary_token = "test"
    )

    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)

    # tidy column names specific to this endpoint
    expect_true(all(c("is_reported", "is_aggregate", "primary_value",
                      "reporter_iso", "partner_iso", "classification_code")
                    %in% names(result)))

    # the endpoint reports the combined SITC classification, not "TM"
    expect_equal(unique(result$classification_code), "SS")
    expect_type(result$is_reported, "logical")
    expect_type(result$primary_value, "double")

    # request metadata survives the World-row filtering
    expect_false(is.null(attr(result, "url")))
  })

  test_that("tidy_cols = FALSE returns the raw column names", {
    result <- comtradr::ct_get_trade_matrix(
      commodity_code = "TOTAL",
      flow_direction = "export",
      reporter = "DEU",
      partner = "FRA",
      start_date = 2023,
      end_date = 2023,
      tidy_cols = FALSE,
      primary_token = "test"
    )

    expect_true(all(c("isReported", "primaryValue") %in% names(result)))
  })
})


# Date validation: frequency is hardcoded annual --------------------------
test_that("monthly-style dates are rejected under annual frequency", {
  # The trade matrix endpoint hardcodes frequency = "A". At the low level,
  # check_date() does not reject a monthly-style "yyyy-mm" input; it coerces it
  # to the containing year rather than raising an error. Document that actual
  # behaviour here.
  expect_equal(
    comtradr:::check_date("2023-01", "2023-01", frequency = "A", bulk = FALSE),
    "2023"
  )

  # ct_get_trade_matrix() guards against this coercion and aborts with a clear
  # message when the user passes anything other than a plain year.
  expect_error(
    comtradr::ct_get_trade_matrix(
      commodity_code = "0",
      flow_direction = "export",
      start_date = "2023-01",
      end_date = "2023-01",
      primary_token = "test"
    ),
    "only provides annual data"
  )
})
