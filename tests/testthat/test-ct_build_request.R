test_that('test that build returns a httr2 request',{
  req <-
    comtradr:::ct_check_params(
      type = 'goods',
      freq = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = "import",
      reporter = "USA",
      partner = "CAN",
      start_date = '2020',
      end_date = '2021',
      partner_2 = 'World',
      mode_of_transport = '0',
      customs_code = 'C00',
      verbose = FALSE,
      update = FALSE
    ) |>
    comtradr:::ct_build_request(primary_token = 'test_token')
  expect_equal(class(req),'httr2_request')

  expect_true(stringr::str_detect(req$url,
                                  '&partnerCode=124'))
  expect_true(stringr::str_detect(req$url,
                                  'https://comtradeapi.un.org/data/v1/get/C/A/HS?'))
})
