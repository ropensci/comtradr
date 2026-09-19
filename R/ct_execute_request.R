#' Split, build, perform and process a Comtrade request
#'
#' Shared request pipeline used by `ct_get_data()` and
#' `ct_get_trade_matrix()`: it splits `params` into chunks that respect the
#' API's URL length limit, builds and performs the requests (optionally
#' cached) and, when `process = TRUE`, combines the processed chunks into a
#' single data.frame.
#'
#' @noRd
ct_execute_request <- function(params,
                               primary_token,
                               requests_per_second,
                               verbose,
                               process,
                               tidy_cols,
                               cache,
                               bulk) {
  ## the API rejects URLs longer than ~2000 characters with HTTP 414, so
  ## requests with long partner/reporter code lists (e.g. `all_countries`)
  ## are split into multiple requests and combined afterwards (issue #103)
  params_list <- ct_split_params(params, primary_token = primary_token)

  if (length(params_list) > 1) {
    if (!process) {
      cli::cli_warn(c("!" = "The request URL exceeds the API's length limit, but it cannot be split into multiple requests with `process = FALSE`, because a single response object must be returned. The request will most likely fail with HTTP error 414; set `process = TRUE` to enable automatic splitting.")) # nolint
      params_list <- list(params)
    } else if (verbose) {
      cli::cli_inform(c("i" = "The request URL exceeds the API's length limit. Splitting into {length(params_list)} requests whose results will be combined.")) # nolint
    }
  }

  reqs <- purrr::map(params_list,
    ct_build_request,
    verbose = verbose,
    primary_token = primary_token,
    bulk = bulk
  )

  if (verbose) {
    cli::cli_inform(c("i" = "Performing request, which can take a few seconds, depending on the amount of data queried.")) # nolint
  }

  if (cache) {
    resps <- purrr::map(reqs,
      ct_perform_request_cache,
      requests_per_second = requests_per_second,
      verbose = verbose,
      bulk = bulk
    )
  } else {
    resps <- purrr::map(reqs,
      ct_perform_request,
      requests_per_second = requests_per_second,
      verbose = verbose,
      bulk = bulk
    )
  }

  if (process) {
    results <- purrr::map(resps,
      ct_process_response,
      verbose = verbose,
      tidy_cols = tidy_cols,
      bulk = bulk
    )

    if (length(results) == 1) {
      return(results[[1]])
    }

    ## chunks without data return the placeholder data.frame(count = 0)
    has_data <- !purrr::map_lgl(results, ~ identical(names(.x), "count"))

    if (!any(has_data)) {
      return(data.frame(count = 0))
    }

    result <- dplyr::bind_rows(results[has_data])
    attributes(result)$url <- purrr::map_chr(resps[has_data], ~ .x$url)
    attributes(result)$time <- Sys.time()
    return(result)
  } else {
    return(resps[[1]])
  }
}
