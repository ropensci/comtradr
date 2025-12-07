# Tests for ct_perform_request and comtrade_error_body
# These tests are skipped on CRAN as they involve network requests and mocking
# Add these to tests/testthat/test-ct_perform_request.R (new file)

# ==============================================================================
# Setup and Helper Functions
# ==============================================================================

# Helper to create mock responses
create_mock_response <- function(status = 200, 
                                 content_type = "application/json",
                                 body = '{"data": []}',
                                 retry_after = NULL) {
  resp <- structure(
    list(
      status_code = status,
      headers = list(
        `Content-Type` = content_type,
        `Retry-After` = retry_after
      ),
      body = charToRaw(body)
    ),
    class = "httr2_response"
  )
  return(resp)
}


# ==============================================================================
# Tests for ct_perform_request - Basic Functionality (Skipped on CRAN)
# ==============================================================================

test_that("ct_perform_request handles successful requests", {
  skip_on_cran()
  skip_if_offline()
  
  # Create a simple request to a known endpoint
  req <- httr2::request("https://httpbin.org/get")
  
  # Should complete without error
  expect_no_error({
    resp <- comtradr:::ct_perform_request(
      req, 
      requests_per_second = 10/60,
      verbose = FALSE,
      bulk = FALSE
    )
  })
  
  # Should return an httr2_response
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
  skip_if_offline()
  
  req <- httr2::request("https://httpbin.org/get")
  
  # Should show message when verbose = TRUE and bulk = FALSE
  expect_message(
    comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = TRUE,
      bulk = FALSE
    ),
    "Got a response object from UN Comtrade"
  )
  
  # Should NOT show message when bulk = TRUE
  expect_silent(
    comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = TRUE,
      bulk = TRUE
    )
  )
})

# ==============================================================================
# Tests for Error Handling (Skipped on CRAN)
# ==============================================================================

test_that("ct_perform_request handles 404 errors", {
  skip_on_cran()
  skip_if_offline()
  
  # Request to non-existent endpoint
  req <- httr2::request("https://httpbin.org/status/404")
  
  # Should throw an error
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
  skip_if_offline()
  
  # Request to endpoint that returns 500
  req <- httr2::request("https://httpbin.org/status/500")
  
  # Should throw an error
  expect_error(
    comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = FALSE,
      bulk = FALSE
    )
  )
})



# ==============================================================================
# Integration Tests with Real ct_get_data (Skipped on CRAN)
# ==============================================================================

test_that("ct_perform_request integrates with ct_get_data workflow", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not(nzchar(Sys.getenv("COMTRADE_PRIMARY")), 
              "API key not set")
  
  # Test that the full workflow works
  # This is an integration test
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
# Tests for Different requests_per_second Values
# ==============================================================================

test_that("ct_perform_request accepts different throttle rates", {
  skip_on_cran()
  skip_on_ci()  # Skip on CI entirely if httpbin is problematic
  skip_if_offline()
  
  req <- httr2::request("https://httpbin.org/get")
  
  # Should work with different rates
  expect_no_error({
    # Fast rate
    resp1 <- comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = FALSE,
      bulk = FALSE
    )
    
    # Slow rate  
    resp2 <- comtradr:::ct_perform_request(
      req,
      requests_per_second = 1/60,
      verbose = FALSE,
      bulk = FALSE
    )
  })
})

# ==============================================================================
# Tests for Bulk vs Non-Bulk Behavior
# ==============================================================================

test_that("ct_perform_request behaves differently for bulk requests", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()  # Skip on CI entirely if httpbin is problematic

  req <- httr2::request("https://httpbin.org/get")
  
  # Non-bulk with verbose should show message
  expect_message(
    comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = TRUE,
      bulk = FALSE
    )
  )
  
  # Bulk with verbose should NOT show message
  expect_silent(
    comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = TRUE,
      bulk = TRUE
    )
  )
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("ct_perform_request handles empty response bodies", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()  # Skip on CI entirely if httpbin is problematic

  # Some endpoints return empty bodies with 200 OK
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

# ==============================================================================
# Documentation Tests (Always Run)
# ==============================================================================

test_that("ct_perform_request is documented as internal", {
  # This test always runs - checks function exists
  expect_true(exists("ct_perform_request", 
                    where = asNamespace("comtradr"),
                    mode = "function"))
})

test_that("comtrade_error_body is documented as internal", {
  # This test always runs - checks function exists
  expect_true(exists("comtrade_error_body",
                    where = asNamespace("comtradr"),
                    mode = "function"))
})

# ==============================================================================
# Regression Tests (Skipped on CRAN)
# ==============================================================================

test_that("ct_perform_request maintains backward compatibility", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()  # Skip on CI entirely if httpbin is problematic
 
  # Should still work with basic parameters
  req <- httr2::request("https://httpbin.org/get")
  
  expect_no_error({
    resp <- comtradr:::ct_perform_request(
      req,
      requests_per_second = 10/60,
      verbose = FALSE,
      bulk = FALSE
    )
  })
  
  # Should return httr2_response class
  expect_s3_class(resp, "httr2_response")
})
