library(testthat)
# Test 'check_type' function
test_that("check_type returns correct type codes and handles invalid inputs", {
  expect_equal(check_type("goods"), c(goods = "C"))
  expect_equal(check_type("services"), c(services = "S"))
  expect_error(check_type("Good"), 'must be one of "goods"')
})

# Test 'check_freq' function
test_that("check_freq returns correct frequency codes and handles invalid inputs", {
  expect_equal(check_freq("C", "A"), "A")
  expect_equal(check_freq("C", "M"), "M")
  expect_equal(check_freq("S", "A"), "A")
  expect_error(check_freq("C", "D"), "`frequency` must be one of")
  expect_error(check_freq("S", "M"), "`frequency` must be one of")
})


library(testthat)

# Test 'check_clCode' function
test_that("check_clCode returns correct classification codes and handles invalid inputs", {
  expect_equal(check_clCode("C", "HS"), "HS")
  expect_equal(check_clCode("S", "B4"), "B4")
  expect_error(check_clCode("C", "ISIC"), "`commodity_classification` must be one of")
  expect_error(check_clCode("S", "HS"), "`commodity_classification` must be one of")
})

# Test 'check_flowCode' function
test_that("check_flowCode returns correct flow codes and handles invalid inputs", {
  expect_equal(check_flowCode("import"), "M")
  expect_equal(check_flowCode(c("export", "re-export")), "X,RX")
  expect_error(check_flowCode("trade"), "`flow_direction` must be one of")
  expect_error(check_flowCode(NULL), "`flow_direction` must be a character vector, not `NULL`")
  expect_error(check_flowCode(c("all", "import")), "You can only provide 'all' as a single argument.")
})

testthat::test_that("check_cmdCode function works correctly", {
  testthat::expect_equal(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = "01",
      update = FALSE,
      verbose = FALSE
    ),
    "01"
  )

  testthat::expect_equal(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = c("01", "02"),
      update = FALSE,
      verbose = FALSE
    ),
    "01,02"
  )

  testthat::expect_error(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = "ABC",
      update = FALSE,
      verbose = FALSE
    )
  )

  testthat::expect_error(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = NULL,
      update = FALSE,
      verbose = FALSE
    )
  )
})

testthat::test_that("check_reporterCode function works correctly", {
  testthat::expect_equal(
    comtradr:::check_reporterCode(
      reporter = "USA",
      update = FALSE,
      verbose = FALSE
    ),
    "842,841"
  )

  testthat::expect_equal(
    comtradr:::check_reporterCode(
      reporter = c("USA", "FRA"),
      update = FALSE,
      verbose = FALSE
    ),
    "251,842,841"
  )

  testthat::expect_true(length(
    comtradr:::check_reporterCode(
      reporter = "all",
      update = FALSE,
      verbose = FALSE
    )
  ) > 0)

  testthat::expect_error(comtradr:::check_reporterCode(
    reporter = NULL,
    update = FALSE,
    verbose = FALSE
  ))
})


testthat::test_that("check_partnerCode works correctly", {
  testthat::expect_equal(check_partnerCode("CAN"), "124")
  testthat::expect_equal(check_partnerCode(c("CAN", "MEX")), "124,484")
  testthat::expect_error(check_partnerCode(c("CAN", "all")))
  testthat::expect_error(check_partnerCode("INVALID"))
  testthat::expect_match(check_partnerCode("all"), "^\\d+(,\\d+)*$")
})

testthat::test_that("check_partner2Code works correctly", {
  testthat::expect_equal(check_partner2Code("CAN"), "124")
  testthat::expect_equal(check_partner2Code(c("CAN", "MEX")), "124,484")
  testthat::expect_error(check_partner2Code(c("CAN", "all")))
  testthat::expect_error(check_partner2Code("INVALID"))
  testthat::expect_match(check_partner2Code("all"), "^\\d+(,\\d+)*$")
})

testthat::test_that("check_motCode works correctly", {
  testthat::expect_equal(check_motCode("2000"), "2000")
  testthat::expect_equal(check_motCode(c("9000", "9100")), "9000,9100")
  testthat::expect_error(check_motCode("INVALID"))
})

testthat::test_that("check_customsCode works correctly", {
  testthat::expect_equal(check_customsCode("C00"), "C00")
  testthat::expect_equal(check_customsCode(c("C01", "C00")), "C01,C00")
  testthat::expect_error(check_customsCode("INVALID"))
})

testthat::test_that("check_date works correctly", {
  testthat::expect_equal(check_date(2010, 2011, "A"), "2010,2011")
  testthat::expect_equal(check_date(2010, 2010, "A"), "2010")
  testthat::expect_equal(check_date("2010-01", "2010-07", "M"), "201001,201002,201003,201004,201005,201006,201007")
  testthat::expect_error(check_date("2010-01", "2011-07", "M"))
  testthat::expect_error(check_date("2010-01", "2011", "M"),"If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.")
})

testthat::test_that("convert_to_date works correctly", {
  testthat::expect_equal(convert_to_date("2010"), as.Date("2010-01-01"))
  testthat::expect_equal(convert_to_date("2010-01"), as.Date("2010-01-01"))
  testthat::expect_equal(convert_to_date("2010-01-01"), as.Date("2010-01-01"))
  testthat::expect_error(convert_to_date("INVALID"))
})
