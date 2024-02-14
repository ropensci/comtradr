test_that('test that build returns a httr2 request',{
  req <-
    comtradr:::ct_check_params(
      type = 'goods',
      freq = 'A',
      commodity_classification = 'HS',
      commodity_code = "01",
      flow_direction = "Import",
      reporter = "USA",
      partner = "CAN",
      start_date = '2020',
      end_date = '2021',
      partner_2 = 'World',
      mode_of_transport = 'Air',
      customs_code = 'C00',
      verbose = FALSE,
      update = FALSE,
      extra_params = NULL, bulk = FALSE
    ) |>
    comtradr:::ct_build_request(primary_token = 'test_token', bulk = FALSE)
  expect_equal(class(req),'httr2_request')

  expect_true(stringr::str_detect(req$url,
                                  '&partnerCode=124'))
  expect_true(stringr::str_detect(req$url,
                            'https://comtradeapi.un.org/data/v1/get/C/A/HS?'))
})

test_that('test that build returns a httr2 request',{

  expect_error(comtradr:::ct_check_params(
    type = 'goods',
    freq = 'A',
    commodity_classification = "HS",
    commodity_code = ct_get_ref_table("HS")$id,
    flow_direction = "Import",
    reporter = "USA",
    partner = "CAN",
    start_date = '2020',
    end_date = '2021',
    partner_2 = 'World',
    mode_of_transport = 'Air',
    customs_code = 'C00',
    verbose = FALSE,
    update = FALSE,
    extra_params = NULL, bulk = FALSE
  ) |>
    comtradr:::ct_build_request(primary_token = 'test_token', bulk = FALSE),
  'Your request exceeds 4KB or 4096 characters')

})
