
httptest2::with_mock_dir("goods",simplify = F, {
  test_that("We can get goods data", {
    expect_s3_class(comtradr::ct_get_data(type = 'goods',
                                        commodity_classification = 'HS',
                                        commodity_code = 'TOTAL',
                                        reporter = 'CHN',
                                        partner = c('ARG','DEU'),
                                        start_date = '2010',
                                        end_date = '2010',
                                        flow_direction = 'all',
                                        primary_token = 'test',
                                        partner_2 = 'World'), 'data.frame')
  })
})

httptest2::with_mock_dir("services",simplify = F, {
  test_that("We can get services data", {
    expect_s3_class(comtradr::ct_get_data(type = 'services',
                                        commodity_classification = 'EB',
                                        commodity_code = '200',
                                        reporter = 'CHN',
                                        partner = c('ARG','DEU'),
                                        start_date = '2010',
                                        end_date = '2010',
                                        flow_direction = 'all',
                                        primary_token = 'test',
                                        partner_2 = 'World'), 'data.frame')
  })
})


without_internet({
  test_that('URL query is correctly constructed with goods',
            {
              expect_GET(
                comtradr::ct_get_data(
                  type = 'goods',
                  commodity_classification = 'HS',
                  commodity_code = 'TOTAL',
                  reporter = 'CHN',
                  partner = c('ARG', 'DEU'),
                  start_date = '2010',
                  end_date = '2010',
                  flow_direction = 'all',
                  primary_token = 'test',
                  partner_2 = 'World'                )
              ,'https://comtradeapi.un.org/data/v1/get/C/A/HS?cmdCode=TOTAL&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE') # nolint
            })
})

without_internet({
  test_that('URL query is correctly constructed with services',
            {
              expect_GET(
                comtradr::ct_get_data(
                  type = 'services',
                  commodity_classification = 'EB',
                  commodity_code = '200',
                  reporter = 'CHN',
                  partner = c('ARG', 'DEU'),
                  start_date = '2010',
                  end_date = '2010',
                  flow_direction = 'all',
                  primary_token = 'test',
                  partner_2 = 'World',verbose = T)
              ,'https://comtradeapi.un.org/data/v1/get/S/A/EB?cmdCode=200&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE') # nolint
            })
})

without_internet({
  test_that('URL query is correctly constructed with services',
            {
              expect_GET(
                comtradr::ct_get_data(
                  type = 'services',
                  commodity_classification = 'EB',
                  commodity_code = '200',
                  reporter = 'CHN',
                  partner = c('ARG', 'DEU'),
                  start_date = '2010',
                  end_date = '2010',
                  flow_direction = 'all',
                  primary_token = 'test',
                  partner_2 = 'World',verbose = T)
                ,'https://comtradeapi.un.org/data/v1/get/S/A/EB?cmdCode=200&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE') # nolint
            })
})

httptest2::with_mock_dir("goods", {
  test_that("test informative message about having gotten data", {
    expect_message(comtradr::ct_get_data(type = 'goods',
                                          commodity_classification = 'HS',
                                          commodity_code = 'TOTAL',
                                          reporter = 'CHN',
                                          partner = c('ARG','DEU'),
                                          start_date = '2010',
                                          end_date = '2010',
                                          verbose = T,
                                          flow_direction = 'all',
                                          primary_token = 'test',
                                          partner_2 = 'World'),'Got a response object from UN Comtrade. Use `process = F` if there is an error' ) # nolint
  })
})


