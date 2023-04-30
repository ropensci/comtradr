
httptest2::with_mock_dir("goods", {
  test_that("We can get goods data", {
    expect_s3_class(comtradr::ct_get_data(type = 'goods',
                                        commodity_classification = 'HS',
                                        commodity_code = 'TOTAL',
                                        reporter = 'CHN',
                                        partner = c('ARG','DEU'),
                                        start_date = '2010',
                                        end_date = '2010',
                                        flow_direction = 'all',
                                        partner_2 = 'World'), 'data.frame')
  })
})

httptest2::with_mock_dir("services", {
  test_that("We can get services data", {
    expect_s3_class(comtradr::ct_get_data(type = 'services',
                                        commodity_classification = 'EB',
                                        commodity_code = '200',
                                        reporter = 'CHN',
                                        partner = c('ARG','DEU'),
                                        start_date = '2010',
                                        end_date = '2010',
                                        flow_direction = 'all',
                                        partner_2 = 'World'), 'data.frame')
  })
})

