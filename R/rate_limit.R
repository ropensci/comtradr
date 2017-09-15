#' Comtradr rate limit check
#'
#' Get the remaining number of queries left in the current hour.
#'
#' @return numeric value, number of current queries left in the hour.
#' @export
#'
#' @examples \dontrun{
#' ct_get_remaining_hourly_queries()
#' }
ct_get_remaining_hourly_queries <- function() {
  get("queries_this_hour", envir = ct_limit_cache)
}

#' Comtradr rate limit time check
#'
#' Get the time in which the hourly limit will reset.
#'
#' @return date and time in which the hourly query limit will reset. Return is
#'   an object of class "POSIXct" (see \code{\link{DateTimeClasses}}).
#' @export
#'
#' @examples \dontrun{
#' ct_get_reset_time()
#'
#' # Get minutes remaining until limit reset, as numeric value.
#' as.double(Sys.time() - ct_get_reset_time())
#' }
ct_get_reset_time <- function() {
  get("next_hour_reset", envir = ct_limit_cache) + 3600
}

#' Comtradr set API token
#'
#' Function to set an API token for the UN Comtrade API. Details on tokens and
#'   rate limits can be found
#'   \url{https://comtrade.un.org/data/doc/api/#Authentication}
#'
#' @param token char string, valid API token.
#'
#' @export
#'
#' @examples \dontrun{
#' ct_register_token("some_valid_token_str")
#' }
ct_register_token <- function(token) {
  # Set token within options.
  ct_options <- getOption("comtradr")
  ct_options$comtrade$token <- token
  ct_options$comtrade$account_type <- "premium"
  ct_options$comtrade$per_hour_limit <- 10000
  class(ct_options) <- "comtradr_credentials"
  options(comtradr = ct_options)

  # Increase the hourly limit within the env ct_limit_cache.
  assign("queries_this_hour", 10000, envir = ct_limit_cache)
}
