#' @title ct_commodity_db_type
#' @description This function is deprecated. There is currently
#' no alternative for this function.
#' `r lifecycle::badge("superseded")`
#' @param ...  Used to catch all possible arguments that users
#' have supplied to this function.
#'
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @export
#' @returns depreciation error
ct_commodity_db_type <- function(...) {
  lifecycle::deprecate_stop("1.0.0", "ct_commodity_db_type()")
}


#' @title ct_country_lookup
#' @description This function is deprecated. You can `use country_codes`
#' to return a dataset with all possible country codes, but in general the
#' specification of iso 3 codes makes a look-up unnecessary.
#' `r lifecycle::badge("superseded")`
#'
#' @inheritParams ct_search
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @export
#' @returns depreciation error
ct_country_lookup <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_country_lookup()"
  )
}

#' @title ct_search
#' @description This function is deprecated Please use  `ct_get_data()` instead.
#' `r lifecycle::badge("superseded")`
#' @param ...  Used to catch all possible arguments that users
#' have supplied to this function.
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @export
#' @returns depreciation error
ct_search <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_search()",
    "ct_get_data()"
  )
}

#' @title ct_update_databases
#' @description This function is deprecated. Please use  `update`
#' parameter in the main `ct_get_data` function instead.
#' `r lifecycle::badge("superseded")`
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @inheritParams ct_search
#' @export
#' @returns depreciation error
ct_update_databases <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_update_databases()"
  )
}

#' @title ct_use_pretty_cols
#' @description This function is deprecated. Please use the `process`
#' argument in the main function instead.
#' `r lifecycle::badge("superseded")`
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @inheritParams ct_search
#' @export
#' @returns depreciation error
ct_use_pretty_cols <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_use_pretty_cols()"
  )
}

#' @title ct_get_reset_time
#' @description This function is deprecated. There is no more reset
#' time, as the upper limit of 250 calls per day is enforced daily.
#' `r lifecycle::badge("superseded")`
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @inheritParams ct_search
#' @export
#' @returns depreciation error
ct_get_reset_time <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_get_reset_time()"
  )
}

#' @title ct_get_remaining_hourly_queries
#' @description This function is deprecated. There is no more reset
#' time, as the upper limit of 250 calls per day is enforced daily.
#' `r lifecycle::badge("superseded")`
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @inheritParams ct_search
#' @export
#' @returns depreciation error
ct_get_remaining_hourly_queries <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_get_remaining_hourly_queries()"
  )
}

#' @title ct_register_token
#' @description This function is deprecated.
#' Please use  `set_primary_comtrade_key()` instead.
#' `r lifecycle::badge("superseded")`
#' @examplesIf interactive()
#' # no examples because only legacy function
#'
#' @inheritParams ct_search
#' @export
#' @returns depreciation error
ct_register_token <- function(...) {
  lifecycle::deprecate_stop(
    "1.0.0",
    "ct_register_token()",
    "set_primary_comtrade_key()"
  )
}
