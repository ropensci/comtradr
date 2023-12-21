# Test 'check_type' function
test_that("check_type returns correct type codes and handles invalid inputs", {
  expect_equal(check_type("goods"), c(goods = "C"))
  expect_equal(check_type("services"), c(services = "S"))
  expect_error(check_type("Good"), 'must be one of "goods"')
})

# Test 'check_freq' function
test_that("check_freq returns correct frequency codes and handles invalid inputs", { # nolint
  expect_equal(check_freq("C", "A"), "A")
  expect_equal(check_freq("C", "M"), "M")
  expect_equal(check_freq("S", "A"), "A")
  expect_error(check_freq("C", "D"), "`frequency` must be one of")
  expect_error(check_freq("S", "M"), "`frequency` must be one of")
})

# Test 'check_clCode' function
test_that("check_clCode returns correct classification codes and handles invalid inputs", {
  expect_equal(check_clCode("C", "HS"), "HS")
  expect_equal(check_clCode("C", "B4"), "B4")
  expect_error(check_clCode("C", "ISIC"), "`commodity_classification` must be one of")
  expect_error(check_clCode("S", "HS"), "`commodity_classification` must be one of")
})

# Test 'check_flowCode' function
test_that("check_flowCode returns correct flow codes and handles invalid inputs", {
  expect_equal(comtradr:::check_flowCode(c("Export", "Re-export"), update = F, verbose = F), "X,RX")
  expect_equal(comtradr:::check_flowCode(c("export", "re-export"), update = F, verbose = F), "X,RX")
  expect_error(comtradr:::check_flowCode("trade", update = F, verbose = F), "`flow_direction` must be one of")
  expect_equal(comtradr:::check_flowCode('everything', update = F, verbose = F),NULL)
})

test_that("check_cmdCode function works correctly", {
  expect_equal(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = "01",
      update = FALSE,
      verbose = FALSE
    ),
    "01"
  )

  expect_equal(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = c("01", "02"),
      update = FALSE,
      verbose = FALSE
    ),
    "01,02"
  )

  expect_error(
    comtradr:::check_cmdCode(
      commodity_classification = "HS",
      commodity_code = "ABC",
      update = FALSE,
      verbose = FALSE
    )
  )
  expect_equal(comtradr:::check_cmdCode(commodity_classification = "HS",
                                        commodity_code = 'everything',
                                        update = F, verbose = F),
               NULL)

})

test_that("check_reporterCode function works correctly", {
  expect_equal(
    comtradr:::check_reporterCode(
      reporter = "USA",
      update = FALSE,
      verbose = FALSE
    ),
    "842,841"
  )
  expect_equal(
    comtradr:::check_reporterCode(
      reporter = "everything",
      update = FALSE,
      verbose = FALSE
    ),
    NULL
  )

  expect_equal(
    comtradr:::check_reporterCode(
      reporter = c("USA", "FRA"),
      update = FALSE,
      verbose = FALSE
    ),
    "251,842,841"
  )

  expect_true(length(
    comtradr:::check_reporterCode(
      reporter = "all_countries",
      update = FALSE,
      verbose = FALSE
    )
  ) > 0)

})

test_that("check_partnerCode works correctly", {
  expect_equal(check_partnerCode("CAN"), "124")
  expect_equal(check_partnerCode(c("CAN", "MEX")), "124,484")
  expect_equal(check_partnerCode(c("everything", "MEX")), NULL)
  expect_error(check_partnerCode(c("CAN", "all")))
  expect_error(check_partnerCode("INVALID"))
  expect_match(check_partnerCode("all_countries"), "^\\d+(,\\d+)*$")
})

test_that("check_partner2Code works correctly", {
  expect_equal(check_partner2Code("CAN"), "124")
  expect_equal(check_partner2Code(c("CAN", "MEX")), "124,484")
  expect_equal(check_partner2Code(c("everything", "MEX")), NULL)
  expect_error(check_partner2Code(c("CAN", "all")))
  expect_error(check_partner2Code("INVALID"))
  expect_match(check_partner2Code("all_countries"), "^\\d+(,\\d+)*$")
})

test_that("check_motCode works correctly", {
  expect_equal(check_motCode("TOTAL modes of transport"), "0")
  expect_equal(check_motCode("everything"), NULL)
  expect_equal(check_motCode("everything","Air"), NULL)
  expect_equal(check_motCode(c("Air", "Water")), "1000,2000")
  expect_error(check_motCode("INVALID"))
  expect_error(check_motCode("INVALID"),"The following mode_of_transport codes you")
})

test_that("check_customsCode works correctly", {
  expect_equal(check_customsCode("C00"), "C00")
  expect_equal(check_customsCode("everything"), NULL)
  expect_equal(check_customsCode("everything",'C00'), NULL)
  expect_equal(check_customsCode(c("C01", "C00")), "C01,C00")
  expect_error(check_customsCode("INVALID"))
  expect_error(check_customsCode("INVALID"),"The following customs_code codes you")
})

test_that("check_date works correctly", {
  expect_equal(check_date(2010, 2011, "A"), "2010,2011")
  expect_equal(check_date(2010, 2010, "A"), "2010")
  expect_equal(check_date("2010-01", "2010-07", "M"),
               "201001,201002,201003,201004,201005,201006,201007")
  expect_error(check_date("2010-01", "2011-07", "M"),
               "If specifying years/months, cannot search more than twelve consecutive years/months in a single query.") # nolint
  expect_error(check_date("2010-01", "2011-07", "M"))
  expect_error(check_date("2010-01", "2011", "M"),
               "If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.") # nolint
})

test_that("convert_to_date works correctly", {
  expect_equal(convert_to_date("2010"), as.Date("2010-01-01"))
  expect_equal(convert_to_date("2010-01"), as.Date("2010-01-01"))
  expect_equal(convert_to_date("2010-01-01"), as.Date("2010-01-01"))
  expect_error(convert_to_date("INVALID"))
})
