test_that("ct_commodity_lookup returns correct values for commodity codes",
          {
            results <- ct_commodity_lookup(c("020820"), return_code = TRUE)
            expect_equal(results, list("020820" = "020820"))
          })

test_that("ct_commodity_lookup returns correct values for commodity names",
          {
            results <- ct_commodity_lookup(c("frogs' legs"),
                                           return_code = FALSE)
            expect_equal(results,
                         list("frogs' legs" = "020820 - - Frogs' legs")) # nolint
          })

test_that("ct_commodity_lookup returns correct values when return_char = TRUE",
          {
            results <-
              ct_commodity_lookup(c("frogs' legs"),
                                  return_code = FALSE,
                                  return_char = TRUE)
            expect_equal(results, c("020820 - - Frogs' legs")) # nolint
          })

test_that("ct_commodity_lookup warns when no matching results are found", {
  expect_warning(ct_commodity_lookup(
    c("Nonexistent"),
    return_code = FALSE,
    verbose = TRUE
  ))
})

test_that("ct_commodity_lookup returns empty result for non-matching search term", # nolint
          {
            results <-
              ct_commodity_lookup(c("Nonexistent"),
                                  return_code = FALSE,
                                  verbose = FALSE)
            expect_equal(results, list("Nonexistent" = character(0)))
          })



# with_mock_dir("ref_table",{
#     test_that("We can get goods data", {
#       # Test 3: Valid dataset_id, update = TRUE, not previously updated
#       test_data_updated <- ct_get_ref_table("HS", update = TRUE)
#       test_data_updated_again <- ct_get_ref_table("HS", update = TRUE)
#       expect_equal(test_data_updated, test_data_updated_again)
#       expect_message(
#         ct_get_ref_table("HS", update = TRUE, verbose = T),
#         'Already checked for updates for HS in this session.'
#       )
# })
#   })

with_mock_dir("ref_table",simplify= F,{
  test_that("ct_get_ref_table works correctly", {
    ct_env <- new.env()
    assign('updated', 'init', envir = ct_env)
    # Test 1: Valid dataset_id, no update
    test_data <- ct_get_ref_table("HS")
    expect_s3_class(test_data, "data.frame")
    # Test 2: Invalid dataset_id
    expect_error(ct_get_ref_table("InvalidID"))
    data <- comtradr:::ct_download_ref_table("cmd_hs")
    test_that("We can get goods data", {
      # Test 3: Valid dataset_id, update = TRUE, not previously updated
      test_data_updated <- ct_get_ref_table("HS", update = TRUE)
      expect_s3_class(test_data, "data.frame")
      expect_true(any(comtradr:::ct_env$updated == "HS"))
      # Test 4: Valid dataset_id, update = TRUE, previously updated
      test_data_updated_again <- ct_get_ref_table("HS", update = TRUE)
      expect_equal(test_data_updated, test_data_updated_again)
      expect_message(
        ct_get_ref_table("HS", update = TRUE, verbose = T),
        'Already checked for updates for HS in this session.'
      )
    })
    # Test 5: Verbose mode
    test_data_verbose <- ct_get_ref_table("HS", verbose = TRUE)
    expect_message(ct_get_ref_table("HS", update = TRUE, verbose = TRUE))
    test_that("ct_search", {
      expect_warning(ct_commodity_lookup(c('cyber', 'tomato')),
                     'There were no matching results found for inputs: cyber')

    })
  })
})

if(length(list.files(tools::R_user_dir('comtradr', which = 'cache')))==0){
  fs::dir_delete(tools::R_user_dir('comtradr', which = 'cache'))
}
if(length(list.files(tools::R_user_dir('comtradr_bulk', which = 'cache')))==0){
  fs::dir_delete(tools::R_user_dir('comtradr_bulk', which = 'cache'))
}
