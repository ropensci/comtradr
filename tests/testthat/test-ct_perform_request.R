# ==============================================================================
# Tests for ct_perform_request - Using httptest2 Mocks
# ==============================================================================

httptest2::with_mock_dir("httpbin", {
  test_that("ct_perform_request handles successful requests", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/get")
    
    expect_no_error({
      resp <- comtradr:::ct_perform_request(
        req, 
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
    })
    
    resp <- comtradr:::ct_perform_request(
      req, 
      requests_per_second = 10/60,
      verbose = FALSE,
      bulk = FALSE
    )
    expect_s3_class(resp, "httr2_response")
  })
  
  test_that("ct_perform_request verbose mode works", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/get")
    
    expect_message(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = TRUE,
        bulk = FALSE
      ),
      "Got a response object from UN Comtrade"
    )
    
    expect_silent(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = TRUE,
        bulk = TRUE
      )
    )
  })
  
  test_that("ct_perform_request handles 404 errors", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/status/404")
    
    expect_error(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
    )
  })
  
  test_that("ct_perform_request handles 500 errors", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/status/500")
    
    expect_error(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
    )
  })
  
  test_that("ct_perform_request accepts different throttle rates", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/get")
    
    expect_no_error({
      resp1 <- comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
      
      resp2 <- comtradr:::ct_perform_request(
        req,
        requests_per_second = 1/60,
        verbose = FALSE,
        bulk = FALSE
      )
    })
  })
  
  test_that("ct_perform_request behaves differently for bulk requests", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/get")
    
    expect_message(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = TRUE,
        bulk = FALSE
      )
    )
    
    expect_silent(
      comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = TRUE,
        bulk = TRUE
      )
    )
  })
  
  test_that("ct_perform_request handles empty response bodies", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/status/200")
    
    expect_no_error({
      resp <- comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
    })
  })
  
  test_that("ct_perform_request maintains backward compatibility", {
    skip_on_cran()
    
    req <- httr2::request("https://httpbin.org/get")
    
    expect_no_error({
      resp <- comtradr:::ct_perform_request(
        req,
        requests_per_second = 10/60,
        verbose = FALSE,
        bulk = FALSE
      )
    })
    
    expect_s3_class(resp, "httr2_response")
  })
})

# ==============================================================================
# Integration Tests with Real ct_get_data (Real Network)
# ==============================================================================

test_that("ct_perform_request integrates with ct_get_data workflow", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("COMTRADE_PRIMARY")), 
              "API key not set")
  
  expect_no_error({
    data <- ct_get_data(
      reporter = "USA",
      partner = "CAN", 
      commodity_code = "TOTAL",
      start_date = 2020,
      end_date = 2020,
      flow_direction = "import"
    )
  })
})

# ==============================================================================
# Documentation Tests (Always Run)
# ==============================================================================

test_that("ct_perform_request is documented as internal", {
  expect_true(exists("ct_perform_request", 
                    where = asNamespace("comtradr"),
                    mode = "function"))
})

test_that("comtrade_error_body is documented as internal", {
  expect_true(exists("comtrade_error_body",
                    where = asNamespace("comtradr"),
                    mode = "function"))
})
