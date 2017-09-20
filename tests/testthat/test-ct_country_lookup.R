context("ct_country_lookup")


test_that("correct vals given reporter lookup", {
  vals <- ct_country_lookup(search_terms = c("Korea", "EU"), type = "reporter")

  # Check values of output.
  expect_equal(vals, c("Dem. People's Rep. of Korea",
                       "EU-28",
                       "Rep. of Korea" ))
})


test_that("correct vals given partner lookup", {
  vals <- ct_country_lookup(search_terms = c("Korea", "EU"), type = "partner")

  # Check values of output.
  expect_equal(vals, c("Africa CAMEU region, nes",
                       "Dem. People's Rep. of Korea",
                       "Eastern Europe, nes",
                       "Europe EFTA, nes",
                       "Europe EU, nes",
                       "Neutral Zone",
                       "Other Europe, nes",
                       "Rep. of Korea" ))
})


test_that("Correct vals given input not found in country DB", {
  expect_equal(ct_country_lookup(search_terms = "not_a_country",
                                 type = "reporter"),
               "No matching results found")
})


test_that("errors and warnings are thrown as expected", {
  # Throw error with invalid input for param "search_terms".
  expect_error(ct_country_lookup(search_terms = 533,
                                 type = "reporter"))

  # Throw error with invalid input for param "type".
  expect_error(ct_country_lookup(search_terms = c("Korea", "EU"),
                                 type = "not_reporter"))
})
