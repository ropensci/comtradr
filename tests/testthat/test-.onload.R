
testthat::test_that("ct_perform_request is cached", {
  testthat::expect_true(memoise::is.memoised(
    comtradr:::ct_perform_request_cache))
  testthat::expect_equal(comtradr:::cache$info()$max_size, 1073741824)
  testthat::expect_equal(comtradr:::cache$info()$max_age, Inf)
  testthat::expect_equal(comtradr:::cache$info()$max_n, Inf)
  testthat::expect_true(dir.exists(tools::R_user_dir('comtradr',
                                                     which = 'cache')))
})


testthat::test_that("ct_perform_request cache parameters are set correctly", {
  script_content <- "
    library(comtradr)
    cache_info <- comtradr:::cache$info()
    cat(paste(cache_info$max_size,
              cache_info$max_age, cache_info$max_n, sep = ','))
  "

  script_file <- tempfile()
  writeLines(script_content, script_file)

  output <- callr::rscript(script_file, env = c(
    COMTRADR_CACHE_MAX_SIZE = '1',
    COMTRADR_CACHE_MAX_AGE = '1',
    COMTRADR_CACHE_MAX_N = '1',
    R_USER_CACHE_DIR = 'cache_test'
  ),show = FALSE)

  cache_values <- strsplit(output$stdout, ',')[[1]]
  cache_values <- as.numeric(cache_values)

  testthat::expect_equal(cache_values[1], 1) # max_size
  testthat::expect_equal(cache_values[2], 1) # max_age
  testthat::expect_equal(cache_values[3], 1) # max_n

  unlink(tools::R_user_dir('comtradr',
                           which = 'cache'), recursive = T)
  unlink("cache_test", recursive = TRUE)
})







