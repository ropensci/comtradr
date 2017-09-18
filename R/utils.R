#' Column names to be used for API data
#'
#' @return char vector of column headers.
#' @noRd
api_col_names <- function() {
  return(c(
    "classification",
    "year",
    "period",
    "period_desc",
    "aggregate_level",
    "is_leaf_code",
    "trade_flow_code",
    "trade_flow",
    "reporter_code",
    "reporter",
    "reporter_iso",
    "partner_code",
    "partner",
    "partner_iso",
    "2nd_partner_code",
    "2nd_partner",
    "2nd_partner_iso",
    "customs_proc_code",
    "customs",
    "mode_of_transport_code",
    "mode_of_transport",
    "commodity_code",
    "commodity",
    "qty_unit_code",
    "qty_unit",
    "qty",
    "alt_qty_unit_code",
    "alt_qty_unit",
    "alt_qty",
    "netweight_kg",
    "gross_weight_kg",
    "trade_value_usd",
    "cif_trade_value_usd",
    "fob_trade_value_usd",
    "flag"
  ))
}


#' Get rate limit values
#'
#' Return the values of pkg env ct_env related to rate limit as a list.
#'
#' @return list containing current rate limit info.
#' @noRd
get_cache_values <- function() {
  last_query <- get("last_query", envir = ct_env)
  if (is.null(last_query)) {
    last_query <- Sys.time()
  }
  list(
    last_query = last_query,
    next_hour_reset = get("next_hour_reset", envir = ct_env),
    queries_this_hour = get("queries_this_hour", envir = ct_env)
  )
}


#' Missing commodity DB file message
#'
#' @noRd
commodity_missing_file_msg <- function() {
  paste0("commodity database file not found. In order to download the ",
         "commodity database from Comtrade, run:\n",
         "ct_update_databases(force = TRUE)")
}


#' Missing country DB file message
#'
#' @noRd
country_missing_file_msg <- function() {
  paste0("country database file not found. In order to download the ",
         "country database from Comtrade, run:\n",
         "ct_update_databases(force = TRUE)")
}
