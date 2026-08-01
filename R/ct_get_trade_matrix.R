#' Get (estimated) trade matrix data from the UN Comtrade API
#'
#' @description
#' This function queries the Trade Matrix endpoint of the UN Comtrade API,
#' an analytical dataset maintained by the UN Statistics Division (UNSD).
#' In contrast to
#' `ct_get_data()`, which returns only what countries have reported, the
#' trade matrix complements reported figures with UNSD estimates, so that
#' the resulting reporter x partner matrix covers world trade completely.
#'
#' `r lifecycle::badge("experimental")`
#'
#' Commodities are classified by SITC (`classification_code` is `"SS"`, the
#' combined SITC used across revisions). The endpoint provides annual data
#' for trade in goods only.
#'
#' @details
#' ## World rows and double counting
#'
#' The endpoint returns aggregate "World" rows (reporter code `0` and/or
#' partner code `0`, ISO `W00`) interleaved with bilateral flows. These are
#' row and column margins of the matrix, plus a grand total. On a full
#' query the bilateral rows, the reporter margins, the partner margins and
#' the grand total each sum to the same world total, so summing the raw
#' response over-counts fourfold.
#'
#' By default `include_world = FALSE` drops these rows, so the result is
#' safe to aggregate. Set `include_world = TRUE` to obtain the margins and
#' the grand total, but do not then sum across the whole frame.
#'
#' Note that `is_aggregate` does **not** identify these rows: the trade
#' matrix endpoint leaves that column unpopulated (`FALSE` on every row,
#' including World rows). It cannot be used to filter aggregates here.
#'
#' ## What `is_reported` means
#'
#' `is_reported` is a **per-cell provenance flag**, not a statement about
#' whether a country reported:
#'
#' * `TRUE` — the value is the reporting country's own figure.
#' * `FALSE` — the cell was produced or adjusted by the UNSD estimation
#'   pipeline: extrapolation from a nearby reported year, mirror inversion
#'   from partner data, manual adjustment of under-reported or confidential
#'   trade, or redistribution of non-specified partners.
#'
#' A country that reports fully can still have many `FALSE` cells, and a
#' `FALSE` value often agrees with the reported figure to within rounding —
#' the flag marks passage through the estimation pipeline, not necessarily a
#' changed number. Commodity aggregates inherit `FALSE` if any constituent
#' detail cell was estimated, so a `TOTAL` row is flagged `FALSE` whenever
#' any underlying section was.
#'
#' A reporter with no `TRUE` cell anywhere is one whose figures here are
#' entirely estimated. To find them, group by reporter and test whether any
#' row is `TRUE`, rather than reading the flag row by row. Note this is not
#' quite the same as "did not report": a country can file with Comtrade and
#' still be estimated throughout this matrix, so check individual cases
#' against `ct_get_data()` when it matters.
#'
#' ## Values
#'
#' Only `primary_value` is populated, in current US dollars, following the
#' usual Comtrade convention (CIF-type for imports, FOB-type for exports).
#' `cifvalue`, `fobvalue`, `qty` and `net_wgt` are always `NA`: the
#' estimation is applied to trade value only, and quantity information is
#' removed from the source data.
#'
#' ## Stability
#'
#' This endpoint is not part of the UN Comtrade public API documentation,
#' and its field semantics may change without notice.
#'
#' @param commodity_code SITC commodity code(s). Accepts `"TOTAL"`, one-,
#' two- and three-digit SITC codes, the five four-/five-digit codes the
#' UN estimates in addition (`"7812"`, `"7841"`, `"7851"`, `"7852"`,
#' `"78531"`), and the level selectors `"ag1"` to `"ag5"`, which return
#' every code at that number of digits. `everything` is a synonym for
#' `"ag1"` (all ten one-digit sections). Use `"all_levels"` to request the
#' entire hierarchy at once — note that levels are nested, so such a result
#' must not be summed. Default: `'TOTAL'`.
#' @param flow_direction The direction of trade flows: `'import'`,
#' `'export'` or `everything` for both. These are the only flows the trade
#' matrix carries. Implemented case-insensitively, so `'import'` and
#' `'Import'` are equivalent. Default: c('import','export').
#' @param reporter Reporter ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` or `comtradr::ct_get_ref_table('reporter')`
#' for possible values. `everything` (the default) returns the complete
#' trade matrix including estimates for non-reporting countries.
#' @param partner Partner ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` for possible values. Default: 'everything'.
#' @param include_world Keep the aggregate World rows (reporter or partner
#' code `0`)? These are the margins and grand total of the matrix and must
#' not be summed together with the bilateral rows. Default: `FALSE`.
#' One exception: if you ask for `partner = "World"` explicitly, that margin
#' is what you requested and is kept, while the reporter margin and grand
#' total are still dropped.
#' @param start_date The start date of the query. The trade matrix endpoint
#' only provides annual data, so this must be a plain year (`yyyy`).
#' @param end_date The end date of the query. Must be a plain year (`yyyy`).
#' The API accepts at most 12 periods per query, so `end_date` may be at
#' most 11 years after `start_date`.
#' @inheritParams ct_get_data
#'
#' @seealso [ct_get_data()] for the standard trade data endpoint, which
#' returns only reported (non-estimated) values. The two are not
#' interchangeable: figures from the trade matrix should not be presented
#' as reported statistics.
#'
#' @references
#' UN Statistics Division, "Note on the Trade Estimation" (2026 update):
#' \url{https://uncomtrade.org/wp-content/uploads/2026/04/Note-on-Trade-Estimation-26-July-2010-2026-Edit-Public.pdf}
#'
#' Product overview, "Trade Matrix - IMTS Analytical Data":
#' \url{https://uncomtrade.org/docs/trade-matrix/}
#'
#' Cite the data source as "UN Comtrade".
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
                                include_world = FALSE,
                                process = TRUE,
                                tidy_cols = TRUE,
                                verbose = FALSE,
                                primary_token = get_primary_comtrade_key(),
                                update = FALSE,
                                requests_per_second = 10 / 60,
                                extra_params = NULL,
                                cache = FALSE) {
  bulk <- FALSE

  if (!rlang::is_logical(include_world, n = 1) || is.na(include_world)) {
    cli::cli_abort("{.arg include_world} must be {.val TRUE} or {.val FALSE}.")
  }

  commodity_code <- check_matrix_cmdCode(commodity_code)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_code."))
  }

  flow_direction <- check_matrix_flowCode(flow_direction, update, verbose)
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

  check_matrix_dates(start_date, end_date)

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

  result <- ct_execute_request(
    params = params,
    primary_token = primary_token,
    requests_per_second = requests_per_second,
    verbose = verbose,
    process = process,
    tidy_cols = tidy_cols,
    cache = cache,
    bulk = bulk
  )

  ## Someone who asked for partner "World" explicitly wants that margin, so
  ## spare the dimension they named -- but keep filtering the other one, or a
  ## `partner = "World"` query would still carry the reporter margin and the
  ## grand total, and double count.
  if (!include_world && isTRUE(process)) {
    result <- drop_world_rows(
      result,
      tidy_cols = tidy_cols,
      drop_reporter = !requests_world(reporter),
      drop_partner = !requests_world(partner),
      verbose = verbose
    )
  }

  result
}

#' Did the user explicitly ask for the World aggregate on this dimension?
#'
#' `check_reporterCode()` / `check_partnerCode()` return `NULL` for
#' `everything` and a comma-separated string of M49 codes otherwise. World is
#' code `0`.
#'
#' @param codes The checked reporter or partner parameter.
#'
#' @returns `TRUE` if code `0` was requested.
#'
#' @noRd
requests_world <- function(codes) {
  if (is.null(codes) || !length(codes)) {
    return(FALSE)
  }

  "0" %in% unlist(strsplit(as.character(codes), ","))
}

#' Check validity of trade matrix commodity code parameter.
#'
#' The trade matrix is estimated at three-digit SITC Rev.3 plus five
#' additional four-/five-digit codes, and aggregated up to `TOTAL`. Codes at
#' any of those levels are valid, as are the `ag1`-`ag5` level selectors.
#'
#' @inheritParams ct_get_trade_matrix
#'
#' @returns A character string of comma-separated codes, or NULL for
#' `all_levels` (which omits the parameter, returning every level).
#'
#' @noRd
check_matrix_cmdCode <- function(commodity_code) {
  ## `all_levels` omits cmdCode entirely, which returns TOTAL plus every
  ## 1-, 2- and 3-digit code at once. Those levels are nested, so the result
  ## must not be summed -- warn rather than let it pass silently.
  if (any(commodity_code %in% "all_levels")) {
    cli::cli_warn(c(
      "{.arg commodity_code} {.val all_levels} returns every level of the \\
      SITC hierarchy at once.",
      "!" = "These levels are nested, so summing {.field primary_value} \\
      across the result will over-count.",
      "i" = "Use {.val ag1} (or {.code everything}) for all one-digit \\
      sections at a single level."
    ))
    return(NULL)
  }

  ## `everything` historically meant "all sections". Omitting cmdCode does
  ## NOT do that -- it returns the whole nested hierarchy -- so map it to the
  ## level selector that actually means all ten one-digit sections.
  if (any(commodity_code %in% "everything")) {
    return("ag1")
  }

  if (!length(commodity_code)) {
    cli::cli_abort(c(
      "{.arg commodity_code} must not be empty.",
      "i" = "Use {.val TOTAL} for all commodities combined, or {.code \\
      everything} for all one-digit SITC sections."
    ))
  }

  commodity_code <- stringr::str_squish(as.character(commodity_code))

  ## `agN` selectors return every code with N digits.
  is_level_selector <- grepl("^ag[1-5]$", commodity_code, ignore.case = TRUE)
  commodity_code[is_level_selector] <-
    tolower(commodity_code[is_level_selector])

  ## Estimation runs at 1-, 2- and 3-digit SITC, plus these five codes.
  ## See the UN "Note on the Trade Estimation".
  extra_codes <- c("7812", "7841", "7851", "7852", "78531")

  valid <- is_level_selector |
    commodity_code == "TOTAL" |
    grepl("^[0-9]{1,3}$", commodity_code) |
    commodity_code %in% extra_codes

  ## An NA anywhere would make `all(valid)` NA and blow up the `if` below.
  valid[is.na(valid)] <- FALSE

  if (!all(valid)) {
    cli::cli_abort(c(
      "The following commodity codes you provided are invalid: \\
      {.val {commodity_code[!valid]}}.",
      "i" = "The trade matrix accepts {.val TOTAL}, one-, two- and \\
      three-digit SITC codes, the codes {.val {extra_codes}}, and the level \\
      selectors {.val {c('ag1', 'ag2', 'ag3', 'ag4', 'ag5')}}."
    ))
  }

  paste0(commodity_code, collapse = ",")
}

#' Check validity of trade matrix flow direction parameter.
#'
#' The trade matrix only carries imports and exports. Other flows in
#' `ct_get_ref_table('flow_direction')` are accepted by the API but return
#' an empty result, so we reject them up front.
#'
#' @inheritParams ct_get_trade_matrix
#'
#' @returns A character string of comma-separated flow codes, or NULL.
#'
#' @noRd
check_matrix_flowCode <- function(flow_direction, update, verbose) {
  supported <- c("import", "export", "everything")
  normalised <- tolower(stringr::str_squish(as.character(flow_direction)))

  ## Validate every element, including alongside `everything`, so a typo is
  ## not silently swallowed by the `everything` short circuit.
  invalid <- is.na(normalised) | !normalised %in% supported

  if (!length(normalised) || any(invalid)) {
    cli::cli_abort(c(
      "The following flow directions are not available from the trade \\
      matrix: {.val {flow_direction[invalid]}}.",
      "i" = "The trade matrix only carries {.val import} and {.val export}, \\
      or {.code everything} for both."
    ))
  }

  if (any(normalised %in% "everything")) {
    return(check_flowCode("everything", update, verbose))
  }

  check_flowCode(normalised, update, verbose)
}

#' Check that trade matrix dates are plain years in a sane order.
#'
#' The trade matrix endpoint only provides annual data. `check_date()` would
#' silently coerce a `"yyyy-mm"` input to the containing year, so we reject
#' anything that is not a plain year explicitly.
#'
#' @inheritParams ct_get_trade_matrix
#'
#' @returns `NULL`, invisibly. Called for its side effect of aborting.
#'
#' @noRd
check_matrix_dates <- function(start_date, end_date) {
  dates <- list(start_date = start_date, end_date = end_date)

  for (arg in names(dates)) {
    date <- dates[[arg]]

    if (is.null(date)) {
      next
    }

    ## Date/POSIXt inputs carry a day and month, so they cannot be a plain
    ## year. Catch them before as.character() turns them into something
    ## unrecognisable in the error message.
    if (inherits(date, c("Date", "POSIXt")) ||
          length(date) != 1L ||
          !is_year(as.character(date))) {
      cli::cli_abort(c(
        "Invalid {.arg {arg}}: {.val {as.character(date)}}.",
        "i" = "The trade matrix endpoint only provides annual data, so \\
        {.arg start_date} and {.arg end_date} must be a single plain year \\
        ({.val yyyy})."
      ))
    }
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    if (as.integer(as.character(start_date)) >
          as.integer(as.character(end_date))) {
      cli::cli_abort(c(
        "{.arg start_date} ({.val {as.character(start_date)}}) is after \\
        {.arg end_date} ({.val {as.character(end_date)}}).",
        "i" = "Provide {.arg start_date} first, or swap the two values."
      ))
    }
  }

  invisible(NULL)
}

#' Drop the aggregate World rows from a trade matrix result.
#'
#' The endpoint interleaves reporter/partner margins and a grand total with
#' the bilateral flows, and flags none of them (`isAggregate` is `FALSE`
#' throughout). Reporter or partner code `0` is the only reliable marker.
#'
#' @param x A processed trade matrix data.frame.
#' @param tidy_cols Were column names tidied?
#' @param drop_reporter Drop rows whose reporter is World?
#' @param drop_partner Drop rows whose partner is World?
#' @param verbose Report how many rows were dropped?
#'
#' @returns `x` without World rows, preserving the `url` and `time`
#' attributes.
#'
#' @noRd
drop_world_rows <- function(x, tidy_cols, drop_reporter = TRUE,
                            drop_partner = TRUE, verbose = FALSE) {
  if (!is.data.frame(x) || nrow(x) == 0 ||
        (!drop_reporter && !drop_partner)) {
    return(x)
  }

  reporter_col <- if (isTRUE(tidy_cols)) "reporter_code" else "reporterCode"
  partner_col <- if (isTRUE(tidy_cols)) "partner_code" else "partnerCode"

  ## An empty response is a placeholder data.frame(count = 0) without these
  ## columns; leave anything unexpected untouched.
  if (!all(c(reporter_col, partner_col) %in% names(x))) {
    return(x)
  }

  keep <- rep(TRUE, nrow(x))
  if (drop_reporter) {
    keep <- keep & as.character(x[[reporter_col]]) != "0"
  }
  if (drop_partner) {
    keep <- keep & as.character(x[[partner_col]]) != "0"
  }
  keep[is.na(keep)] <- TRUE

  if (verbose) {
    cli::cli_inform(c(
      "v" = "Dropped {sum(!keep)} aggregate World row{?s}. Set \\
      {.code include_world = TRUE} to keep them."
    ))
  }

  result <- x[keep, , drop = FALSE]
  rownames(result) <- NULL

  ## `[.data.frame` drops the request metadata ct_process_response() attaches.
  for (attribute in c("url", "time")) {
    attr(result, attribute) <- attr(x, attribute)
  }

  result
}
