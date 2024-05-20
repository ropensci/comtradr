# Test 'check_type' function
test_that("check_type returns correct type codes and handles invalid inputs", {
  expect_equal(comtradr:::check_type("goods"), c(goods = "C"))
  expect_equal(comtradr:::check_type("services"), c(services = "S"))
  expect_error(comtradr:::check_type("Good"), 'must be one of "goods"')
})

# Test 'check_freq' function
test_that("check_freq returns correct frequency codes and handles invalid inputs", { # nolint
  expect_equal(comtradr:::check_freq("C", "A"), "A")
  expect_equal(comtradr:::check_freq("C", "M"), "M")
  expect_equal(comtradr:::check_freq("S", "A"), "A")
  expect_error(comtradr:::check_freq("C", "D"), "`frequency` must be one of")
  expect_error(comtradr:::check_freq("S", "M"), "`frequency` must be one of")
})

# Test 'check_clCode' function
test_that("check_clCode returns correct classification codes and handles invalid inputs", { #nolint
  expect_equal(comtradr:::check_clCode("C", "HS", bulk = FALSE), "HS")
  expect_equal(comtradr:::check_clCode("C", "H5", bulk = TRUE), "H5")
  expect_equal(comtradr:::check_clCode("C", "B4", bulk = FALSE), "B4")
  expect_error(comtradr:::check_clCode("C", "ISIC", bulk = FALSE),
               "`commodity_classification` must be one of")
  expect_error(comtradr:::check_clCode("S", "HS", bulk = FALSE),
               "`commodity_classification` must be one of")
})

# Test 'check_flowCode' function
test_that("check_flowCode returns correct flow codes and handles invalid inputs", { # nolint
  expect_equal(comtradr:::check_flowCode(c("Export", "Re-export"),
                                         update = F, verbose = F), "X,RX")
  expect_equal(comtradr:::check_flowCode(c("export", "re-export"),
                                         update = F, verbose = F), "X,RX")
  expect_error(comtradr:::check_flowCode("trade", update = F, verbose = F),
               "`flow_direction` must be one of")
  expect_equal(comtradr:::check_flowCode('everything', update = F,
                                         verbose = F),NULL)
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
  expect_error(comtradr:::check_reporterCode("INVALID"),
               regexp = "The following reporter")
  expect_error(comtradr:::check_reporterCode(c("all_countries","USA")),
               "can only be provided")

})

test_that("check_partnerCode works correctly", {
  expect_equal(comtradr:::check_partnerCode("CAN"), "124")
  expect_equal(comtradr:::check_partnerCode(c("CAN", "MEX")), "124,484")
  expect_equal(comtradr:::check_partnerCode(c("everything", "MEX")), NULL)
  expect_error(comtradr:::check_partnerCode(c("CAN", "all")))
  expect_error(comtradr:::check_partnerCode("INVALID"))
  expect_error(comtradr:::check_partnerCode(c("all_countries","USA")),
               "can only be provided")
  expect_match(comtradr:::check_partnerCode("all_countries"), "^\\d+(,\\d+)*$")
})

test_that("check_partner2Code works correctly", {
  expect_equal(comtradr:::check_partner2Code("CAN"), "124")
  expect_equal(comtradr:::check_partner2Code(c("CAN", "MEX")), "124,484")
  expect_equal(comtradr:::check_partner2Code(c("everything", "MEX")), NULL)
  expect_error(comtradr:::check_partner2Code(c("CAN", "all")))
  expect_error(comtradr:::check_partner2Code("INVALID"))
  expect_error(comtradr:::check_partner2Code(c("all_countries","USA")),
               "can only be provided")
  expect_match(comtradr:::check_partner2Code("all_countries"), "^\\d+(,\\d+)*$")
})

test_that("check_motCode works correctly", {
  expect_equal(comtradr:::check_motCode("TOTAL modes of transport"), "0")
  expect_equal(comtradr:::check_motCode("everything"), NULL)
  expect_equal(comtradr:::check_motCode("everything","Air"), NULL)
  expect_equal(comtradr:::check_motCode(c("Air", "Water")), "1000,2000")
  expect_error(comtradr:::check_motCode("INVALID"))
  expect_error(comtradr:::check_motCode("INVALID"),
               "The following mode_of_transport codes you")
  expect_equal(
    comtradr:::check_motCode("everything")
  , NULL)
})

test_that("check_customsCode works correctly", {
  expect_equal(comtradr:::check_customsCode("C00"), "C00")
  expect_equal(comtradr:::check_customsCode("everything"), NULL)
  expect_equal(comtradr:::check_customsCode("everything",'C00'), NULL)
  expect_equal(comtradr:::check_customsCode(c("C01", "C00")), "C01,C00")
  expect_error(comtradr:::check_customsCode("INVALID"))
  expect_error(comtradr:::check_customsCode("INVALID"),
               "The following customs_code codes you")
})

test_that("check_date works correctly", {
  expect_equal(check_date(2010, 2011, "A", bulk = FALSE), "2010,2011")
  expect_equal(check_date(2010, 2011, "A", bulk = TRUE), "2010,2011")
  expect_equal(check_date(2000, 2024, "A", bulk = TRUE),
               "2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024") # nolint
  expect_equal(check_date(2010, 2010, "A", bulk = FALSE), "2010")
  expect_equal(check_date("2010-01", "2010-07", "M", bulk = FALSE),
               "201001,201002,201003,201004,201005,201006,201007")
  expect_equal(check_date("2010-01", "2020-01", "M", bulk = TRUE),
               "201001,201002,201003,201004,201005,201006,201007,201008,201009,201010,201011,201012,201101,201102,201103,201104,201105,201106,201107,201108,201109,201110,201111,201112,201201,201202,201203,201204,201205,201206,201207,201208,201209,201210,201211,201212,201301,201302,201303,201304,201305,201306,201307,201308,201309,201310,201311,201312,201401,201402,201403,201404,201405,201406,201407,201408,201409,201410,201411,201412,201501,201502,201503,201504,201505,201506,201507,201508,201509,201510,201511,201512,201601,201602,201603,201604,201605,201606,201607,201608,201609,201610,201611,201612,201701,201702,201703,201704,201705,201706,201707,201708,201709,201710,201711,201712,201801,201802,201803,201804,201805,201806,201807,201808,201809,201810,201811,201812,201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,202001") #nolint
  expect_error(check_date("2010-01", "2011-07", "M", bulk = FALSE),
               "If specifying years/months, cannot search more than twelve consecutive years/months in a single query.") # nolint
  expect_error(check_date("2010-01", "2011-07", "M", bulk = FALSE))
  expect_error(check_date("2010-01", "2011", "M", bulk = FALSE),
               "If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.") # nolint
  expect_error(check_date("2010-01", "2011", "M", bulk = TRUE),
               "If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.") # nolint
})

test_that("convert_to_date works correctly", {
  expect_equal(convert_to_date("2010"), as.Date("2010-01-01"))
  expect_equal(convert_to_date("2010-01"), as.Date("2010-01-01"))
  expect_equal(convert_to_date("2010-01-01"), as.Date("2010-01-01"))
  expect_error(convert_to_date("INVALID"))
})
