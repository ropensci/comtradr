library(comtradr)
library(httptest2)
library(testthat)


httptest2::with_mock_dir("goods_bulk_error", {
  test_that("test that error works for params that have no values", {
    expect_error(
      comtradr::ct_get_bulk(
        reporter = 'ARG',
        start_date = '1920',
        end_date = "1920",
        primary_token = 'test',
      ),
      "Probably no data for this combination of parameters"
    )
  })
})

without_internet({
  test_that('URL query is correctly constructed with goods',
            {
              expect_GET(
                comtradr::ct_get_bulk(
                  reporter = 'ARG',
                  start_date = '1962',
                  end_date = "1962",
                  primary_token = 'test'
                ),
                'https://comtradeapi.un.org/bulk/v1/get/C/A/HS?reporterCode=32&period=1962') # nolint
            })
})

httptest2::set_redactor(function(x) {
  httptest2::gsub_response(x, "https\\://comtradeapi.un.org/bulk/v1/file/32/",
                           "")
})

httptest2::with_mock_dir('../goods_bulk', {
  test_that('returns a data.frame',
            {
              expect_s3_class(
                comtradr::ct_get_bulk(
                  reporter = 'ARG',
                  start_date = '1962',
                  end_date = "1962",
                  primary_token = 'test',
                  cache = FALSE
                ),
                'data.frame'
              )
            })

  test_that('returns correct verbose messages',

            {
              captured_messages <- capture_messages({
                comtradr::ct_get_bulk(
                  reporter = 'ARG',
                  start_date = '1962',
                  end_date = "1962",
                  primary_token = 'test',
                  verbose = TRUE
                )
              })

              # Then you can use expect_match to verify the specific messages
              expect_true(any(grepl(
                "Processing bulk file", captured_messages
              )))
              expect_true(any(grepl(
                "Performing request", captured_messages
              )))
              expect_true(any(grepl(
                "Will download files", captured_messages
              )))
             })
})


test_that("tidy cols are returned for bulk",{
  test <- try(get_primary_comtrade_key(), silent = TRUE)
  skip_if(any(class(test) %in% c("rlang_error", "error", "try-error")))
  tidy <- comtradr::ct_get_bulk(
    reporter = 'ARG',
    start_date = '1962',
    end_date = "1962",
    primary_token = get_primary_comtrade_key(),
    cache = FALSE
  )
  not_tidy <- comtradr::ct_get_bulk(
    reporter = 'ARG',
    start_date = '1962',
    end_date = "1962",
    tidy_cols = F,
    primary_token = get_primary_comtrade_key(),
    cache = FALSE
  )
  expect_true(all(names(tidy) %in% comtradr::ct_pretty_cols$to))
  expect_true(!all(names(not_tidy) %in% comtradr::ct_pretty_cols$to))
})
