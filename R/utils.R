#' Column names to be used for API data
#'
#' @return char vector of column headers.
#' @noRd
api_col_names <- function() {
  return(c(
    "Classification",
    "Year",
    "Period",
    "Period Desc.",
    "Aggregate Level",
    "Is Leaf Code",
    "Trade Flow Code",
    "Trade Flow",
    "Reporter Code",
    "Reporter",
    "Reporter ISO",
    "Partner Code",
    "Partner",
    "Partner ISO",
    "2nd Partner Code",
    "2nd Partner",
    "2nd Partner ISO",
    "Customs Proc. Code",
    "Customs",
    "Mode of Transport Code",
    "Mode of Transport",
    "Commodity Code",
    "Commodity",
    "Qty Unit Code",
    "Qty Unit",
    "Qty",
    "Alt Qty Unit Code",
    "Alt Qty Unit",
    "Alt Qty",
    "Netweight (kg)",
    "Gross weight (kg)",
    "Trade Value (US$)",
    "CIF Trade Value (US$)",
    "FOB Trade Value (US$)",
    "Flag"
  ))
}


#' Get rate limit values
#'
#' Return the values of pkg env ct_env related to rate limit as a list.
#'
#' @return list containing current rate limit info.
#' @noRd
get_cache_values <- function() {
  list(
    last_query = get("last_query", envir = ct_env),
    next_hour_reset = get("next_hour_reset", envir = ct_env),
    queries_this_hour = get("queries_this_hour", envir = ct_env)
  )
}
