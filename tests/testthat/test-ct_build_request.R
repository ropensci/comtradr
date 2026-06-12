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

test_that('split_codes packs codes within the encoded-length budget', {
  codes <- paste(sprintf('%03d', 1:100), collapse = ',')
  chunks <- comtradr:::split_codes(codes, budget = 100)

  encoded_length <- function(x) nchar(x) + 2 * stringr::str_count(x, ',')
  expect_true(all(sapply(chunks, encoded_length) <= 100))

  ## concatenating the chunks reproduces the original codes in order
  expect_identical(paste(unlist(chunks), collapse = ','), codes)
})

test_that('split_codes handles NULL and oversized single codes', {
  expect_identical(comtradr:::split_codes(NULL, 100), list(NULL))
  expect_identical(comtradr:::split_codes('12', 100), list('12'))
  ## a single code larger than the budget still gets its own chunk
  expect_identical(comtradr:::split_codes('123456', 3), list('123456'))
})

test_that('ct_split_params splits long partner code lists into URL-safe chunks', { # nolint
  params <- comtradr:::ct_check_params(
    type = 'goods',
    frequency = 'M',
    commodity_classification = 'HS',
    commodity_code = '847010',
    flow_direction = 'import',
    reporter = 'DEU',
    partner = 'all_countries',
    start_date = 2012,
    end_date = 2012,
    partner_2 = 'World',
    mode_of_transport = 'TOTAL modes of transport',
    customs_code = 'C00',
    verbose = FALSE,
    update = FALSE,
    extra_params = NULL, bulk = FALSE
  )

  params_list <- comtradr:::ct_split_params(params, primary_token = 'test')
  expect_gt(length(params_list), 1)

  urls <- sapply(params_list, function(p) {
    comtradr:::ct_build_request(p, primary_token = 'test', bulk = FALSE)$url
  })
  expect_true(all(nchar(urls) <= 2048))

  ## all partner codes are preserved across the chunks
  partner_codes <- sapply(params_list,
                          function(p) p$query_params$partnerCode)
  expect_identical(paste(partner_codes, collapse = ','),
                   params$query_params$partnerCode)

  ## the reporter codes stay intact in every chunk
  expect_true(all(sapply(params_list, function(p) {
    identical(p$query_params$reporterCode, params$query_params$reporterCode)
  })))
})

test_that('ct_split_params covers all reporter x partner combinations', {
  params <- comtradr:::ct_check_params(
    type = 'goods',
    frequency = 'M',
    commodity_classification = 'HS',
    commodity_code = '847010',
    flow_direction = 'import',
    reporter = 'all_countries',
    partner = 'all_countries',
    start_date = 2012,
    end_date = 2012,
    partner_2 = 'World',
    mode_of_transport = 'TOTAL modes of transport',
    customs_code = 'C00',
    verbose = FALSE,
    update = FALSE,
    extra_params = NULL, bulk = FALSE
  )

  params_list <- comtradr:::ct_split_params(params, primary_token = 'test')

  reporter_chunks <- unique(sapply(params_list,
                                   function(p) p$query_params$reporterCode))
  partner_chunks <- unique(sapply(params_list,
                                  function(p) p$query_params$partnerCode))

  ## both parameters were split and every combination of reporter chunk and
  ## partner chunk is queried, so no reporter-partner pair is lost
  expect_gt(length(reporter_chunks), 1)
  expect_gt(length(partner_chunks), 1)
  expect_equal(length(params_list),
               length(reporter_chunks) * length(partner_chunks))

  pairs <- unique(sapply(params_list, function(p) {
    paste(p$query_params$reporterCode, p$query_params$partnerCode, sep = '|')
  }))
  expect_equal(length(pairs),
               length(reporter_chunks) * length(partner_chunks))

  ## every individual (reporter code, partner code) pair is queried exactly
  ## once across all chunks: none lost, none duplicated
  code_pairs <- unlist(lapply(params_list, function(p) {
    r <- strsplit(p$query_params$reporterCode, ",", fixed = TRUE)[[1]]
    pt <- strsplit(p$query_params$partnerCode, ",", fixed = TRUE)[[1]]
    as.vector(outer(r, pt, paste, sep = "|"))
  }))
  expected_pairs <- as.vector(outer(
    strsplit(params$query_params$reporterCode, ",", fixed = TRUE)[[1]],
    strsplit(params$query_params$partnerCode, ",", fixed = TRUE)[[1]],
    paste,
    sep = "|"
  ))
  expect_identical(anyDuplicated(code_pairs), 0L)
  expect_setequal(code_pairs, expected_pairs)

  urls <- sapply(params_list, function(p) {
    comtradr:::ct_build_request(p, primary_token = 'test', bulk = FALSE)$url
  })
  expect_true(all(nchar(urls) <= 2048))
})

test_that('ct_split_params leaves short requests unchanged', {
  params <- comtradr:::ct_check_params(
    type = 'goods',
    frequency = 'A',
    commodity_classification = 'HS',
    commodity_code = '01',
    flow_direction = 'Import',
    reporter = 'USA',
    partner = 'CAN',
    start_date = '2020',
    end_date = '2021',
    partner_2 = 'World',
    mode_of_transport = 'Air',
    customs_code = 'C00',
    verbose = FALSE,
    update = FALSE,
    extra_params = NULL, bulk = FALSE
  )

  expect_identical(comtradr:::ct_split_params(params, primary_token = 'test'),
                   list(params))
})
