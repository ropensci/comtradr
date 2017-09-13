context("ct_country_lookup")

# All tests on the expected return data.
test_that("lookup return values are correct, and fail when expected", {
  #skip_on_cran()
  #skip_on_travis()

  df <- ct_countries_table()
  Sys.sleep(3)

  # Correct return values for reporter countries lookup.
  expect_equal(ct_country_lookup(loc = c("Korea", "EU"),
                                 type = "reporter",
                                 lookuptable = df),
               c("Dem. People's Rep. of Korea",
                 "EU-28",
                 "Rep. of Korea" ))

  # Correct return values for partner countries lookup.
  expect_equal(ct_country_lookup(loc = c("Korea", "EU"),
                                 type = "partner",
                                 lookuptable = df),
               c("Africa CAMEU region, nes",
                 "Dem. People's Rep. of Korea",
                 "Eastern Europe, nes",
                 "Europe EFTA, nes",
                 "Europe EU, nes",
                 "Neutral Zone",
                 "Other Europe, nes",
                 "Rep. of Korea" ))

  # Correct return values when input for "loc" not found in lookup table.
  expect_equal(ct_country_lookup(loc = "not_a_country",
                                 type = "reporter",
                                 lookuptable = df),
               "No matching results found")

  # Throw error with invalid input for param "loc".
  expect_error(ct_country_lookup(loc = 533,
                                 type = "reporter",
                                 lookuptable = df))

  # Throw error with invalid input for param "type".
  expect_error(ct_country_lookup(loc = c("Korea", "EU"),
                                 type = "not_reporter",
                                 lookuptable = df))

  # Throw error with invalid input for param "lookuptable".
  expect_error(ct_country_lookup(loc = c("Korea", "EU"),
                                 type = "reporter",
                                 lookuptable = "lookuptable"))
})
