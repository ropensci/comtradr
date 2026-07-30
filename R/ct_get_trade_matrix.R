#' Get (estimated) trade matrix data from the UN Comtrade API
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function queries the Trade Matrix endpoint of the UN Comtrade API.
#' In contrast to `ct_get_data()`, the returned data includes estimates for
#' countries that have not (yet) reported their trade data, so that the
#' resulting reporter x partner matrix covers world trade completely.
#' Commodities are classified by one-digit SITC sections ("0" to "9") or
#' "TOTAL". The endpoint only provides annual data for trade in goods.
#'
#' @details
#' Rows with `is_reported == FALSE` (tidy column name) contain estimated
#' values, `is_aggregate` marks aggregated rows. The classification code
#' returned is "TM".
#'
#' @param commodity_code One-digit SITC section code(s) ("0" to "9"),
#' "TOTAL" for the sum of all sections or `everything` for all sections.
#' Default: 'TOTAL'.
#' @param flow_direction The direction of trade flows or `everything`.
#' Possible values can be found in `ct_get_ref_table('flow_direction')`. These
#' are implemented case-insensitive, 'import' and 'Import' are equivalent.
#' Default: c('import','export').
#' @param reporter Reporter ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` or `comtradr::ct_get_ref_table('reporter')`
#' for possible values. `everything` (the default) returns the complete
#' trade matrix including estimates for non-reporting countries.
#' @param partner Partner ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` for possible values. Default: 'everything'.
#' @param start_date The start date of the query, `yyyy`.
#' @param end_date The end date of the query, `yyyy`.
#' Max: 12 years after start date.
#' @inheritParams ct_get_data
#'
#' @examplesIf interactive()
#' ## World export matrix for food and live animals (SITC section 0) in 2023,
#' ## including estimates for non-reporting countries
#' ct_get_trade_matrix(
#'   commodity_code = "0",
#'   flow_direction = "export",
#'   start_date = 2023,
#'   end_date = 2023
#' )
#'
#' @export
#' @returns A data.frame with trade matrix data or,
#' if `process = F`, a httr2 response object.
ct_get_trade_matrix <- function(commodity_code = "TOTAL",
                                flow_direction = c("import", "export"),
                                reporter = "everything",
                                partner = "everything",
                                start_date = NULL,
                                end_date = NULL,
                                process = TRUE,
                                tidy_cols = TRUE,
                                verbose = FALSE,
                                primary_token = get_primary_comtrade_key(),
                                update = FALSE,
                                requests_per_second = 10 / 60,
                                extra_params = NULL,
                                cache = FALSE) {
  bulk <- FALSE

  commodity_code <- check_matrix_cmdCode(commodity_code)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_code."))
  }

  flow_direction <- check_flowCode(flow_direction, update, verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of flow_direction."))
  }

  reporter <- check_reporterCode(reporter, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of reporter."))
  }

  partner <- check_partnerCode(partner, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of partner."))
  }

  ## the trade matrix endpoint only provides annual data
  period <- check_date(start_date, end_date, frequency = "A", bulk = bulk)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of start and end dates."))
  }

  params <- list(
    query_params = list(
      cmdCode = commodity_code,
      flowCode = flow_direction,
      partnerCode = partner,
      reporterCode = reporter,
      period = period,
      includeDesc = "TRUE"
    ),
    url_params = list(
      type = "C",
      freq = "A",
      clCode = "TM",
      endpoint = "getTradeMatrix"
    ),
    extra_params = list(
      extra_params = extra_params
    )
  )

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

#' Check validity of trade matrix commodity code parameter.
#'
#' The trade matrix endpoint classifies commodities by one-digit SITC
#' sections ("0" to "9") or "TOTAL".
#'
#' @inheritParams ct_get_trade_matrix
#'
#' @returns A character string of comma-separated codes, or NULL for
#' `everything`.
#'
#' @noRd
check_matrix_cmdCode <- function(commodity_code) {
  if (any(commodity_code %in% "everything")) {
    return(NULL)
  }

  commodity_code <- stringr::str_squish(as.character(commodity_code))
  valid_codes <- c("TOTAL", as.character(0:9))

  if (!all(commodity_code %in% valid_codes)) {
    rlang::abort(paste0(
      "The following commodity codes you provided are invalid: ",
      paste0(setdiff(commodity_code, valid_codes), collapse = ", "),
      ". The trade matrix endpoint only accepts one-digit SITC sections ",
      '("0" to "9"), "TOTAL" or "everything".'
    ))
  }

  paste0(commodity_code, collapse = ",")
}
