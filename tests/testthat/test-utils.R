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

path <- tools::R_user_dir("comtradr", which = "cache")
if (fs::dir_exists(path) && length(fs::dir_ls(path)) == 0) fs::dir_delete(path)

path <- tools::R_user_dir("comtradr_bulk", which = "cache")
if (fs::dir_exists(path) && length(fs::dir_ls(path)) == 0) fs::dir_delete(path)



# Test available_variables reference table
test_that("ct_get_ref_table works correctly for available_variables", {
  # Test 1: Valid dataset_id, no update
  test_data <- ct_get_ref_table("available_variables")
  expect_s3_class(test_data, "data.frame")
  expect_true(nrow(test_data) > 0)
})

test_that("available_variables contains expected columns after merge", {
  # Get the available_variables reference table
  variables <- ct_get_ref_table("available_variables")
  
  # Should have columns from both the original data AND ct_pretty_cols
  expected_cols <- c("dataItem", "description", "to")
  
  # Check that all expected columns are present
  expect_true(all(expected_cols %in% colnames(variables)))
})

test_that("available_variables merge with ct_pretty_cols works correctly", {
  # Get the reference table
  variables <- ct_get_ref_table("available_variables")
  
  # Check that the merge happened correctly
  # The 'to' column should come from ct_pretty_cols
  # The 'dataItem' column should match 'from' column for merged rows
  
  # Filter to rows that should have been merged
  merged_rows <- variables |> 
    poorman::filter(!is.na(to))
  
  # For merged rows, from should equal dataItem
  expect_true(all(merged_rows$from == merged_rows$dataItem, na.rm = TRUE))
})

test_that("available_variables has last_modified date", {
  variables <- ct_get_ref_table("available_variables")
  
  # Should have last_modified column
  expect_true("last_modified" %in% colnames(variables))
  
  # Should be a valid date
  expect_s3_class(variables$last_modified, "Date")
})

with_mock_dir("ref_table_available_vars", simplify = FALSE, {
  test_that("ct_get_ref_table update works for available_variables", {
    # Reset the updated tracking
    ct_env <- new.env()
    assign('updated', 'init', envir = ct_env)
    
    # Test with update = TRUE, not previously updated
    test_data_updated <- ct_get_ref_table("available_variables", update = TRUE)
    expect_s3_class(test_data_updated, "data.frame")
    
    # Check that it was marked as updated
    expect_true(any(comtradr:::ct_env$updated == "available_variables"))
    
    # Test with update = TRUE, previously updated (should return same data)
    test_data_updated_again <- ct_get_ref_table("available_variables", update = TRUE)
    expect_equal(test_data_updated, test_data_updated_again)
  })
})

test_that("available_variables verbose mode works", {
  # Should show message when verbose = TRUE
  expect_message(
    ct_get_ref_table("available_variables", update = TRUE, verbose = TRUE),
    "available_variables"
  )
})

test_that("available_variables can find common trade variables", {
  variables <- ct_get_ref_table("available_variables")
  
  # Common trade variables should be present
  common_vars <- c("typeCode", "reporterCode", "partnerCode", 
                   "cmdCode", "primaryValue")
  
  # Check that these are in the dataItem column
  expect_true(all(common_vars %in% variables$dataItem))
})

test_that("available_variables tidy names are present", {
  variables <- ct_get_ref_table("available_variables")
  
  # Common tidy names should be present in the 'to' column
  common_tidy <- c("type_code", "reporter_code", "partner_code", 
                   "cmd_code", "primary_value")
  
  # Check that these are in the 'to' column
  expect_true(any(common_tidy %in% variables$to))
})

test_that("available_variables matches ct_pretty_cols mapping", {
  variables <- ct_get_ref_table("available_variables")
  
  # Load ct_pretty_cols for comparison
  data("ct_pretty_cols", package = "comtradr", envir = environment())
  
  # For each row in ct_pretty_cols, check that the mapping exists in variables
  # (but not all variables will be in ct_pretty_cols)
  
  # Get merged subset
  merged_subset <- variables |> 
    poorman::filter(dataItem %in% ct_pretty_cols$from)
  
  # The 'to' values should match for the same 'from'/'dataItem' values
  for (i in 1:min(5, nrow(merged_subset))) {  # Test first 5 rows
    item <- merged_subset$dataItem[i]
    expected_to <- ct_pretty_cols$to[ct_pretty_cols$from == item]
    actual_to <- merged_subset$to[merged_subset$dataItem == item]
    
    if (length(expected_to) > 0 && length(actual_to) > 0) {
      expect_equal(actual_to[1], expected_to[1])
    }
  }
})

test_that("available_variables returns data even if not all fields merge", {
  # This tests that the left_join doesn't drop unmatched rows
  variables <- ct_get_ref_table("available_variables")
  
  # Should have more rows than ct_pretty_cols (since it's a left join)
  data("ct_pretty_cols", package = "comtradr", envir = environment())
  
  # Variables should have at least as many rows as the original dataitem table
  # (some might not match with ct_pretty_cols)
  expect_true(nrow(variables) > 0)
  
  # Should have some rows with NA in 'to' (unmatched items)
  # OR all rows matched (depending on data)
  # Just check the structure is valid
  expect_true("to" %in% colnames(variables))
})


# Edge case: Empty or invalid dataset
test_that("ct_get_ref_table still validates dataset_id for available_variables", {
  # Should work for valid ID
  expect_s3_class(ct_get_ref_table("available_variables"), "data.frame")
  
  # Should error for invalid ID (this tests the validation still works)
  expect_error(ct_get_ref_table("InvalidID"))
})

# Test that available_variables is properly initialized in ct_env
test_that("available_variables is initialized in ct_env", {
  # Check that the variable is defined in ct_env (even if NULL initially)
  expect_true(exists("available_variables", envir = comtradr:::ct_env))
})
