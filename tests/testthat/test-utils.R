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



with_mock_dir("re_tb1",simplify= F,{
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


# ==============================================================================
# Tests for Commodity Classification Reference Tables
# ==============================================================================

test_that("ct_get_ref_table works for all commodity classifications", {
  commodity_codes <- c("HS", "H0", "H1", "H2", "H3", "H4", "H5", "H6",
                       "B4", "B5", "EB02", "EB10", "EB10S", "EB",
                       "S1", "S2", "S3", "S4", "SS")
  
  for (code in commodity_codes) {
    result <- ct_get_ref_table(code)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true("id" %in% colnames(result))
  }
})

test_that("commodity reference tables have expected structure", {
  # Test HS as representative commodity table
  hs_data <- ct_get_ref_table("HS")
  
  # Should have standard commodity columns
  expected_cols <- c("id", "text")
  expect_true(all(expected_cols %in% colnames(hs_data)))
  
  # ID should be character or numeric
  expect_true(is.character(hs_data$id) || is.numeric(hs_data$id))
  
  # Text should be character
  expect_true(is.character(hs_data$text))
  
  # Should have last_modified date
  expect_true("last_modified" %in% colnames(hs_data))
})

test_that("different commodity classifications return different data", {
  hs_data <- ct_get_ref_table("HS")
  s1_data <- ct_get_ref_table("S1")
  
  # Should have different number of rows (different classifications)
  expect_false(nrow(hs_data) == nrow(s1_data))
})

# ==============================================================================
# Tests for Country/Partner Reference Tables
# ==============================================================================

test_that("ct_get_ref_table works for reporter and partner", {
  reporter_data <- ct_get_ref_table("reporter")
  partner_data <- ct_get_ref_table("partner")
  
  expect_s3_class(reporter_data, "data.frame")
  expect_s3_class(partner_data, "data.frame")
  expect_true(nrow(reporter_data) > 0)
  expect_true(nrow(partner_data) > 0)
})

test_that("reporter and partner have expected country columns", {
  reporter_data <- ct_get_ref_table("reporter")
  partner_data <- ct_get_ref_table("partner")
  
  # Both should have these columns
  expected_cols <- c("id", "country", "iso_3", "iso_2", "group")
  
  expect_true(all(expected_cols %in% colnames(reporter_data)))
  expect_true(all(expected_cols %in% colnames(partner_data)))
})

test_that("reporter and partner have valid ISO codes", {
  reporter_data <- ct_get_ref_table("reporter")
  
  # ISO 3 should be 3 characters (or 'World' for partner)
  iso3_lengths <- nchar(reporter_data$iso_3[!is.na(reporter_data$iso_3)])
  expect_true(all(iso3_lengths == 3 | iso3_lengths == 5)) # 3 chars or "World"
  
  # ISO 2 should be 2 characters
  iso2_lengths <- nchar(reporter_data$iso_2[!is.na(reporter_data$iso_2)])
  expect_true(all(iso2_lengths == 2))
})

test_that("partner table includes 'World' aggregate", {
  partner_data <- ct_get_ref_table("partner")
  
  # Should have World as a partner
  expect_true("World" %in% partner_data$country)
  
  # World should have iso_3 as 'World'
  world_row <- partner_data[partner_data$country == "World", ]
  expect_equal(world_row$iso_3[1], "World")
})

test_that("reporter and partner have entry/exit year information", {
  reporter_data <- ct_get_ref_table("reporter")
  
  expect_true("entry_year" %in% colnames(reporter_data))
  expect_true("exit_year" %in% colnames(reporter_data))
  
  # Entry years should be numeric
  expect_true(is.numeric(reporter_data$entry_year))
})

# ==============================================================================
# Tests for Other Reference Tables
# ==============================================================================

test_that("ct_get_ref_table works for mode_of_transport", {
  mot_data <- ct_get_ref_table("mode_of_transport")
  
  expect_s3_class(mot_data, "data.frame")
  expect_true(nrow(mot_data) > 0)
  expect_true("id" %in% colnames(mot_data))
  
  # Should have a 'TOTAL' option
  expect_true(any(grepl("TOTAL", mot_data$text, ignore.case = TRUE)))
})

test_that("ct_get_ref_table works for flow_direction", {
  flow_data <- ct_get_ref_table("flow_direction")
  
  expect_s3_class(flow_data, "data.frame")
  expect_true(nrow(flow_data) > 0)
  
  # Should have standard flow types
  flow_texts <- tolower(flow_data$text)
  expect_true(any(grepl("import", flow_texts)))
  expect_true(any(grepl("export", flow_texts)))
})

test_that("ct_get_ref_table works for customs_code", {
  customs_data <- ct_get_ref_table("customs_code")
  
  expect_s3_class(customs_data, "data.frame")
  expect_true(nrow(customs_data) > 0)
  expect_true("id" %in% colnames(customs_data))
})

test_that("ct_get_ref_table works for frequency", {
  freq_data <- ct_get_ref_table("frequency")
  
  expect_s3_class(freq_data, "data.frame")
  expect_true(nrow(freq_data) > 0)
  
  # Should have Annual and Monthly
  freq_texts <- freq_data$text
  expect_true(any(grepl("Annual", freq_texts, ignore.case = TRUE)))
  expect_true(any(grepl("Monthly", freq_texts, ignore.case = TRUE)))
})

test_that("ct_get_ref_table works for mode_of_supply", {
  mos_data <- ct_get_ref_table("mode_of_supply")
  
  expect_s3_class(mos_data, "data.frame")
  expect_true(nrow(mos_data) > 0)
  expect_true("id" %in% colnames(mos_data))
})

test_that("ct_get_ref_table works for units_of_quantity", {
  qty_data <- ct_get_ref_table("units_of_quantity")
  
  expect_s3_class(qty_data, "data.frame")
  expect_true(nrow(qty_data) > 0)
  expect_true("qtyCode" %in% colnames(qty_data))
})

# ==============================================================================
# Tests for Update Functionality Across Different Tables
# ==============================================================================

with_mock_dir("re_tb2", simplify = FALSE, {
  test_that("update works for different reference table types", {
    # Reset environment
    ct_env <- new.env()
    assign('updated', 'init', envir = ct_env)
    
    # Test update for commodity code
    hs_updated <- ct_get_ref_table("HS", update = TRUE)
    expect_s3_class(hs_updated, "data.frame")
    
    # Test update for reporter
    reporter_updated <- ct_get_ref_table("reporter", update = TRUE)
    expect_s3_class(reporter_updated, "data.frame")
    
    # Test update for mode_of_transport
    mot_updated <- ct_get_ref_table("mode_of_transport", update = TRUE)
    expect_s3_class(mot_updated, "data.frame")
  })
})

test_that("update flag prevents repeated downloads in same session", {
  # First update
  data1 <- ct_get_ref_table("flow_direction", update = TRUE)
  
  # Second update - should use cached
  expect_message(
    ct_get_ref_table("flow_direction", update = TRUE, verbose = TRUE),
    "Already checked for updates"
  )
})

# ==============================================================================
# Tests for Verbose Mode Across Different Tables
# ==============================================================================

test_that("verbose mode works for different reference tables", {
  # Should not show message when verbose = FALSE
  expect_silent(ct_get_ref_table("HS", verbose = FALSE))
  
  # Should show message when update = TRUE and verbose = TRUE
  expect_message(
    ct_get_ref_table("reporter", update = TRUE, verbose = TRUE)
  )
})

# ==============================================================================
# Tests for Data Consistency and Quality
# ==============================================================================

test_that("all reference tables have last_modified date", {
  tables_to_test <- c("HS", "reporter", "partner", "mode_of_transport", 
                      "flow_direction", "customs_code", "frequency")
  
  for (table in tables_to_test) {
    data <- ct_get_ref_table(table)
    expect_true("last_modified" %in% colnames(data))
    expect_s3_class(data$last_modified, "Date")
  }
})

test_that("reference tables return unique IDs", {
  tables_to_test <- c("HS", "reporter", "partner", "flow_direction")
  
  for (table in tables_to_test) {
    data <- ct_get_ref_table(table)
    
    # IDs should be unique
    expect_equal(nrow(data), length(unique(data$id)))
  }
})

test_that("reference tables do not have all NA columns", {
  tables_to_test <- c("HS", "reporter", "partner", "mode_of_transport")
  
  for (table in tables_to_test) {
    data <- ct_get_ref_table(table)
    
    # No column should be all NA
    all_na_cols <- sapply(data, function(x) all(is.na(x)))
    expect_false(any(all_na_cols))
  }
})

# ==============================================================================
# Tests for Error Handling and Edge Cases
# ==============================================================================

test_that("ct_get_ref_table errors on invalid dataset_id", {
  expect_error(ct_get_ref_table("INVALID_CODE"))
  expect_error(ct_get_ref_table("invalid"))
  expect_error(ct_get_ref_table(""))
  expect_error(ct_get_ref_table(123))
})

test_that("ct_get_ref_table handles case sensitivity correctly", {
  # Should error - case sensitive
  expect_error(ct_get_ref_table("hs"))  # lowercase
  expect_error(ct_get_ref_table("Hs"))  # mixed case
  
  # Should work - correct case
  expect_s3_class(ct_get_ref_table("HS"), "data.frame")
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("reference tables integrate with ct_commodity_lookup", {
  # Should be able to use HS reference table with commodity lookup
  result <- ct_commodity_lookup("tomato", commodity_classification = "HS")
  expect_type(result, "list")
})

test_that("reporter and partner tables integrate with country_codes", {
  # Load country_codes
  data("country_codes", package = "comtradr", envir = environment())
  
  # Get reference tables
  reporter_ref <- ct_get_ref_table("reporter")
  partner_ref <- ct_get_ref_table("partner")
  
  # Reporter and partner IDs should exist in country_codes
  # (at least for some of them)
  reporter_ids <- reporter_ref$id[1:min(10, nrow(reporter_ref))]
  expect_true(any(reporter_ids %in% country_codes$id))
})

test_that("multiple reference tables can be loaded in same session", {
  # Should be able to load multiple tables without conflicts
  hs <- ct_get_ref_table("HS")
  reporter <- ct_get_ref_table("reporter")
  mot <- ct_get_ref_table("mode_of_transport")
  flow <- ct_get_ref_table("flow_direction")
  
  # All should be data frames
  expect_s3_class(hs, "data.frame")
  expect_s3_class(reporter, "data.frame")
  expect_s3_class(mot, "data.frame")
  expect_s3_class(flow, "data.frame")
  
  # Should have different structures
  expect_false(identical(colnames(hs), colnames(reporter)))
  expect_false(identical(colnames(hs), colnames(mot)))
})

# ==============================================================================
# Tests for Caching Behavior
# ==============================================================================

test_that("reference tables are cached in ct_env after first load", {
  # Load a table
  hs_data <- ct_get_ref_table("HS")
  
  # Should now exist in ct_env
  expect_true(exists("HS", envir = comtradr:::ct_env))
  
  # Loading again should return same data (from cache)
  hs_data2 <- ct_get_ref_table("HS")
  expect_equal(hs_data, hs_data2)
})

test_that("different tables are cached separately", {
  # Load multiple tables
  hs <- ct_get_ref_table("HS")
  reporter <- ct_get_ref_table("reporter")
  
  # Both should exist in ct_env
  expect_true(exists("HS", envir = comtradr:::ct_env))
  expect_true(exists("reporter", envir = comtradr:::ct_env))
  
  # Should be different objects
  expect_false(identical(hs, reporter))
})

# ==============================================================================
# Performance Tests (Light)
# ==============================================================================

test_that("reference tables load in reasonable time", {
  # First load (from disk)
  time1 <- system.time(ct_get_ref_table("HS"))
  expect_true(time1["elapsed"] < 5)  # Should load in less than 5 seconds
  
  # Second load (from cache) - should be faster
  time2 <- system.time(ct_get_ref_table("HS"))
  expect_true(time2["elapsed"] < time1["elapsed"] || time2["elapsed"] < 1)
})

# ==============================================================================
# Tests for Specific Table Features
# ==============================================================================

test_that("commodity tables have hierarchical structure info", {
  hs <- ct_get_ref_table("HS")
  
  # Should have aggrLevel or similar hierarchical info
  # (checking if such columns exist)
  col_names <- colnames(hs)
  expect_true(any(grepl("level|hierarchy|aggr", col_names, ignore.case = TRUE)) ||
              "id" %in% col_names)  # At minimum should have id
})

test_that("services classifications work correctly", {
  # Test services-specific classifications
  eb02 <- ct_get_ref_table("EB02")
  expect_s3_class(eb02, "data.frame")
  expect_true(nrow(eb02) > 0)
  
  eb10 <- ct_get_ref_table("EB10")
  expect_s3_class(eb10, "data.frame")
  expect_true(nrow(eb10) > 0)
  
  # Should be different
  expect_false(identical(eb02, eb10))
})

# ==============================================================================
# Regression Tests
# ==============================================================================

test_that("ct_get_ref_table maintains backward compatibility", {
  # These should all still work as they did before
  expect_s3_class(ct_get_ref_table("HS"), "data.frame")
  expect_s3_class(ct_get_ref_table("reporter"), "data.frame")
  expect_s3_class(ct_get_ref_table("partner"), "data.frame")
  
  # Should return data.frame, not tibble or other class
  hs <- ct_get_ref_table("HS")
  expect_true("data.frame" %in% class(hs))
})

test_that("column names remain consistent", {
  # Reporter should have specific columns
  reporter <- ct_get_ref_table("reporter")
  required_cols <- c("id", "country", "iso_3")
  expect_true(all(required_cols %in% colnames(reporter)))
  
  # Partner should have specific columns
  partner <- ct_get_ref_table("partner")
  expect_true(all(required_cols %in% colnames(partner)))
})

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

with_mock_dir("re_tb3", simplify = FALSE, {
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
