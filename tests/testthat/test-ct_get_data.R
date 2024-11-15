
httptest2::with_mock_dir("goods",simplify = F, {
  test_that("We can get goods data", {
    expect_s3_class(comtradr::ct_get_data(type = 'goods',
                                        commodity_classification = 'HS',
                                        commodity_code = 'TOTAL',
                                        reporter = 'CHN',
                                        partner = c('ARG','DEU'),
                                        start_date = '2010',
                                        end_date = '2010',
                                        flow_direction = 'everything',
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
                                        flow_direction = 'everything',
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
                  flow_direction = c('Import','Export','Re-export','Re-import'),
                  primary_token = 'test',
                  partner_2 = 'World'                )
              ,'https://comtradeapi.un.org/data/v1/get/C/A/HS?cmdCode=TOTAL&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE') # nolint
            })

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
                  primary_token = 'test',
                  partner_2 = 'World')
              ,'https://comtradeapi.un.org/data/v1/get/S/A/EB?cmdCode=200&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE') # nolint
            })
})



httptest2::with_mock_dir("data", {
  test_that("test informative message about having gotten data", {
    captured_messages <- capture_messages({
      comtradr::ct_get_data(
        type = 'goods',
        commodity_classification = 'HS',
        commodity_code = 'TOTAL',
        reporter = 'CHN',
        partner = c('ARG', 'DEU'),
        start_date = '2010',
        end_date = '2010',
        verbose = TRUE,
        primary_token = 'test',
        partner_2 = 'World'
      )
    })

    # Check if the desired message is part of the output
    expect_true(any(grepl("Got a response object from UN Comtrade. Use `process = F` if there is an error", captured_messages))) # nolint
  })
})



test_that("tidy cols are returned",{
  test <- try(get_primary_comtrade_key(), silent = TRUE)
  skip_if(any(class(test) %in% c("rlang_error", "error", "try-error")))
  tidy <- comtradr::ct_get_data(
    type = 'goods',
    commodity_classification = 'HS',
    commodity_code = 'TOTAL',
    reporter = 'CHN',
    partner = 'DEU',
    start_date = '2010',
    end_date = '2010',
    primary_token = get_primary_comtrade_key(),
    partner_2 = 'World'
  )
  not_tidy <- comtradr::ct_get_data(
    type = 'goods',
    commodity_classification = 'HS',
    commodity_code = 'TOTAL',
    reporter = 'CHN',
    partner =  'DEU',
    start_date = '2010',
    end_date = '2010',
    tidy_cols = FALSE,
    primary_token = get_primary_comtrade_key(),
    partner_2 = 'World'
  )
  expect_true(all(names(tidy) %in% comtradr::ct_pretty_cols$to))
  expect_true(!all(names(not_tidy) %in% comtradr::ct_pretty_cols$to))
})




