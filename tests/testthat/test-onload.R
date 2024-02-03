
testthat::test_that("ct_perform_request is cached", {
  testthat::expect_true(memoise::is.memoised(
    comtradr:::ct_perform_request_cache))
  testthat::expect_equal(comtradr:::cache$info()$max_size, 1073741824)
  testthat::expect_equal(comtradr:::cache$info()$max_age, Inf)
  testthat::expect_equal(comtradr:::cache$info()$max_n, Inf)
  testthat::expect_true(dir.exists(rappdirs::user_cache_dir('comtradr')))
})

testthat::test_that("ct_perform_request is cached", {
  detach("package:comtradr", unload = TRUE)
  Sys.setenv('COMTRADR_CACHE_MAX_SIZE' = 1)
  Sys.setenv('COMTRADR_CACHE_MAX_AGE' = 1)
  Sys.setenv('COMTRADR_CACHE_MAX_N' = 1)
  Sys.setenv('R_USER_CACHE_DIR' = 'cache_test')
  library(comtradr)
  testthat::expect_true(memoise::is.memoised(
    comtradr:::ct_perform_request_cache))
  testthat::expect_equal(comtradr:::cache$info()$dir,
                         stringr::str_c(getwd(),
                                        '/cache_test/comtradr'))
  testthat::expect_equal(comtradr:::cache$info()$max_size, 1)
  testthat::expect_equal(comtradr:::cache$info()$max_age, 1)
  testthat::expect_equal(comtradr:::cache$info()$max_n, 1)
  testthat::expect_true(dir.exists(rappdirs::user_cache_dir('comtradr')))
})


