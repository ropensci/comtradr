
testthat::test_that("ct_perform_request is cached", {
  testthat::expect_true(memoise::is.memoised(
    comtradr:::ct_perform_request_cache))
  testthat::expect_equal(comtradr:::cache$info()$max_size, 1073741824)
  testthat::expect_equal(comtradr:::cache$info()$max_age, Inf)
  testthat::expect_equal(comtradr:::cache$info()$max_n, Inf)
  testthat::expect_true(dir.exists(rappdirs::user_cache_dir('comtradr')))
})



testthat::test_that("ct_perform_request is cached", {
  testthat::expect_equal({
    sc <- tempfile()
    cat("library(comtradr)\ncomtradr:::cache$info()$dir", file = sc)
    sub <- callr::rscript(sc, env = c(
      COMTRADR_CACHE_MAX_SIZE = "1",
      COMTRADR_CACHE_MAX_AGE = "1",
      COMTRADR_CACHE_MAX_N = "1",
      R_USER_CACHE_DIR = "cache_test"
    ))
    stringr::str_extract(sub$stdout, '"/.*comtradr') |>
      stringr::str_remove('^"') |>
      stringr::str_remove('/$')
  }, stringr::str_c(getwd(), '/cache_test/comtradr'))
})


testthat::test_that("ct_perform_request cache parameters are set correctly", {
  script_content <- "
    library(comtradr)
    cache_info <- comtradr:::cache$info()
    cache_info_list <- list(
      max_size = cache_info$max_size,
      max_age = cache_info$max_age,
      max_n = cache_info$max_n
    )
    cat(jsonlite::toJSON(cache_info_list))
  "

  script_file <- tempfile()
  writeLines(script_content, script_file)

  output <- callr::rscript(script_file, env = c(
    COMTRADR_CACHE_MAX_SIZE = "1",
    COMTRADR_CACHE_MAX_AGE = "1",
    COMTRADR_CACHE_MAX_N = "1",
    R_USER_CACHE_DIR = "cache_test"
  ))

  cache_info <- jsonlite::fromJSON(output$stdout)

  testthat::expect_equal(cache_info$max_size, 1)
  testthat::expect_equal(cache_info$max_age, 1)
  testthat::expect_equal(cache_info$max_n, 1)
})






