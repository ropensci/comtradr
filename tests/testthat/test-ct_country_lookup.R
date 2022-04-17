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
  expect_equal(sort(vals), c("Africa CAMEU region, nes",
                             "Dem. People's Rep. of Korea",
                             "Eastern Europe, nes",
                             "Europe EFTA, nes",
                             "Europe EU, nes",
                             "Neutral Zone",
                             "Other Europe, nes",
                             "Rep. of Korea" ))
})


test_that("correct vals given input not found in country DB", {
  expect_equal(ct_country_lookup(search_terms = "not_a_country",
                                 type = "reporter"),
               "No matching results found")
})


test_that("correct vals given some invalid inputs", {
  vals <- ct_country_lookup(search_terms = c("Korea", NA, "EU"), type = "reporter")

  # Check values of output.
  expect_equal(vals, c("Dem. People's Rep. of Korea",
                       "EU-28",
                       "Rep. of Korea" ))
})


test_that("throw error with invalid input to arg 'search_terms'", {
  expect_error(ct_country_lookup(search_terms = 533,
                                 type = "reporter"))
})


test_that("throw error with invalid input to arg 'type'", {
  expect_error(ct_country_lookup(search_terms = c("Korea", "EU"),
                                 type = "not_reporter"))
})


test_that("throw warning when ignore.case = TRUE and fixed = TRUE", {
  expect_warning(ct_country_lookup(search_terms = "korea",
                                   ignore.case = TRUE, fixed = TRUE))
})
