
httptest2::with_mock_dir("goods_bulk_error", {
  test_that("test that error works for params that have no values", {
    expect_error(
      ct_get_bulk(
        reporter = 'ARG',
        start_date = '1920',
        end_date = "1920"
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
                                      verbose = T)
                ,'https://comtradeapi.un.org/bulk/v1/get/C/A/HS?reporterCode=32&period=1962') # nolint
            })
})

httptest2::with_mock_dir("goods_bulk",{
  test_that("We can get goods data", {
    expect_s3_class(comtradr::ct_get_bulk(reporter = 'ARG',
                                          start_date = '1962',
                                          end_date = "1962",
                                          verbose = T), 'data.frame')
  })
})

httptest2::with_mock_dir("goods_bulk", {
  test_that("test informative messages", {
    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         verbose = T),
                   'Processing bulk file, this writes to your cache directory') # nolint
    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         verbose = T),
                   'Performing request, which can take a few seconds, depend') # nolint

    expect_message(comtradr::ct_get_bulk(reporter = 'ARG',
                                         start_date = '1962',
                                         end_date = "1962",
                                         verbose = T),
                   'Will download files size of: 183.3 KB') # nolint
  })
})
