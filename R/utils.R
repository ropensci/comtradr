#' Column names to be used for API data
#'
#' @param type char string, the type of the column headers to use. "desc" will
#'  return headers that are descriptive and easy to interpret. "comtrade" will
#'  return the col headers that are used by the UN Comtrade API. Default
#'  value is "desc".
#' @return char vector of column headers.
#' @noRd
api_col_names <- function(type = c("desc", "comtrade")) {
  type <- match.arg(type)

  if (type == "desc") {
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
  } else {
    return(c(
      "pfCode",
      "yr",
      "period",
      "periodDesc",
      "aggrLevel",
      "IsLeaf",
      "rgCode",
      "rgDesc",
      "rtCode",
      "rtTitle",
      "rt3ISO",
      "ptCode",
      "ptTitle",
      "pt3ISO",
      "ptCode2",
      "ptTitle2",
      "pt3ISO2",
      "cstCode",
      "cstDesc",
      "motCode",
      "motDesc",
      "cmdCode",
      "cmdDescE",
      "qtCode",
      "qtDesc",
      "qtAltCode",
      "qtAltDesc",
      "TradeQuantity",
      "AltQuantity",
      "NetWeight",
      "GrossWeight",
      "TradeValue",
      "CIFValue",
      "FOBValue",
      "estCode"
    ))
  }
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


#' Get commodity database
#'
#' Helper function that attempts to return the commodity DB as a data frame.
#' It first looks in the pkg env "ct_env", if that fails it will look for the
#' pkg file "commodity_table.rda". If both fail an error will be thrown
#' telling the user how to download the commodity DB file (just a matter of
#' running ct_update_databases(force = TRUE)).
#'
#' @noRd
get_commodity_db <- function() {
  # Look in ct_env.
  df <- get("commodity_df", envir = ct_env)
  if (!is.null(df)) {
    return(df)
  } else {
    # If no commodity_df in ct_env, look for commodity_table file.
    commodity_file <- system.file("extdata",
                                  "commodity_table.rda",
                                  package = "comtradr")
    if (file.exists(commodity_file)) {
      load(commodity_file, envir = ct_env)
      df <- get("commodity_df", envir = ct_env)
      return(df)
    } else {
      stop(missing_file_msg("commodity"), call. = FALSE)
    }
  }
}


#' Get country database
#'
#' Helper function that will attempt to return the country DB as a data frame.
#' It first looks in the pkg env "ct_env", if that fails it will look for the
#' pkg file "commodity_table.rda". If both fail an error will be thrown
#' telling the user how to download the commodity DB file (just a matter of
#' running ct_update_databases(force = TRUE)).
#'
#' @noRd
get_country_db <- function() {
  # Look in ct_env.
  df <- get("country_df", envir = ct_env)
  if (!is.null(df)) {
    return(df)
  } else {
    # If no country_df in ct_env, look for country_table file.
    country_file <- system.file("extdata",
                                "country_table.rda",
                                package = "comtradr")
    if (file.exists(country_file)) {
      load(country_file, envir = ct_env)
      df <- get("country_df", envir = ct_env)
      return(df)
    } else {
      stop(missing_file_msg("country"), call. = FALSE)
    }
  }
}


#' Missing DB file message
#'
#' @noRd
missing_file_msg <- function(type) {
  paste0(type, " database file not found. In order to download the ",
         type, " database from Comtrade, run:\n",
         "ct_update_databases(force = TRUE)")
}
