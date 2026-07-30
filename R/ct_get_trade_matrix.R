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
#' returned is "TM". When `tidy_cols = TRUE`, you can, for example, filter
#' `is_reported == FALSE` to inspect the estimated rows or `is_aggregate ==
#' FALSE` to drop aggregates.
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
#' @param start_date The start date of the query. The trade matrix endpoint
#' only provides annual data, so this must be a plain year (`yyyy`).
#' @param end_date The end date of the query. Must be a plain year (`yyyy`).
#' Max: 12 years after start date.
#' @inheritParams ct_get_data
#'
#' @seealso [ct_get_data()] for the standard trade data endpoint, which
#' returns only reported (non-estimated) values.
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

  ## the trade matrix endpoint only provides annual data, so only plain years
  ## are accepted. check_date() would silently coerce "yyyy-mm" inputs to the
  ## containing year, which we reject explicitly here.
  for (date in c(start_date, end_date)) {
    if (!is.null(date) && !is_year(as.character(date))) {
      cli::cli_abort(c(
        "Invalid date {.val {date}}.",
        "i" = "The trade matrix endpoint only provides annual data, so \\
        {.arg start_date} and {.arg end_date} must be a plain year \\
        ({.val yyyy})."
      ))
    }
  }

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

  ct_execute_request(
    params = params,
    primary_token = primary_token,
    requests_per_second = requests_per_second,
    verbose = verbose,
    process = process,
    tidy_cols = tidy_cols,
    cache = cache,
    bulk = bulk
  )
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
