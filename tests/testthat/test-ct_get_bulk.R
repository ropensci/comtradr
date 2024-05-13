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
                comtradr::ct_get_bulk(reporter = 'ARG',
                                      start_date = '1962',
                                      end_date = "1962",
                                      primary_token = 'test',
                                      verbose = T)
                ,'https://comtradeapi.un.org/bulk/v1/get/C/A/HS?reporterCode=32&period=1962') # nolint
            })
})

httptest2::set_redactor(
  function(x){
    httptest2::gsub_response(x, "https\\://comtradeapi.un.org/bulk/v1/file/32/",
                             "")
    })

options(httptest2.verbose = TRUE)


httptest2::with_mock_dir('../goods_bulk',{
    expect_s3_class(comtradr::ct_get_bulk(reporter = 'ARG',
                                          start_date = '1962',
                                          end_date = "1962",
                                          primary_token = 'test',
                                          verbose = T), 'data.frame')
})

httptest2::with_mock_dir("../goods_bulk",{
    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         primary_token = 'test',
                                         verbose = T),
                   'Processing bulk file, this writes to your cache directory') # nolint
    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         primary_token = 'test',
                                         verbose = T),
                   'Performing request, which can take a few seconds, depend') # nolint

    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         primary_token = 'test',
                                         verbose = T),
                   'Will download files') # nolint
})
