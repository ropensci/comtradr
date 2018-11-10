#' Comtradr rate limit check
#'
#' Get the remaining number of queries left in the current hour.
#'
#' @return numeric value, number of current queries left in the hour.
#' @export
#'
#' @examples
#' ct_get_remaining_hourly_queries()
ct_get_remaining_hourly_queries <- function() {
  # If the hourly time limit has past, reset the hourly time limt and hourly
  # query count.
  ct_get_reset_time()
  get("queries_this_hour", envir = ct_env)
}


#' Comtradr rate limit time check
#'
#' Get the time in which the hourly limit will reset.
#'
#' @param set logical, if \code{TRUE} and the current reset time is
#'   \code{NULL}, set the reset time to be one hour from the current
#'   \code{Sys.time}.
#'
#' @return date and time in which the hourly query limit will reset. Return is
#'   a "POSIXct" object (see \code{\link{DateTimeClasses}}).
#' @export
#'
#' @examples
#' ct_get_reset_time()
#'
#' # Get minutes remaining until limit reset, as numeric value.
#' as.double(ct_get_reset_time() - Sys.time())
ct_get_reset_time <- function(set = NULL) {
  stopifnot(is.null(set) || is.logical(set))
  val <- get("next_hour_reset", envir = ct_env)
  if (is.null(val)) {
    if (isTRUE(set)) {
      # If val is NULL and set is TRUE, reset the time limit in the pkg env,
      # then return new value.
      reset_hourly_limits()
      return(get("next_hour_reset", envir = ct_env))
    }
    # If val is NULL and set is not TRUE, return current time plus one hour.
    return(Sys.time() + 3600)
  }
  if (Sys.time() > val) {
    # If "next_hour_reset" time limit has expired, reset the
    # time limit in the pkg env, then return new value.
    reset_hourly_limits()
    return(get("next_hour_reset", envir = ct_env))
  }

  # Else if the time limit has not yet expired, return the current value.
  return(val)
}


#' Reset Hourly Limits
#'
#' Reset the "next_hour_reset" and "queries_this_hour" values in the pkg env.
#'
#' @noRd
reset_hourly_limits <- function() {
  assign("next_hour_reset", Sys.time() + 3600, envir = ct_env)
  assign("queries_this_hour", getOption("comtradr")$comtrade$per_hour_limit,
         envir = ct_env)
}


#' Comtradr set API token
#'
#' Function to set an API token for the UN Comtrade API. Details on tokens and
#'   rate limits can be found
#'   \url{https://comtrade.un.org/data/doc/api/#Authentication}
#'
#' @param token char string, valid API token.
#'
#' @return Set comtradr API token and update rate limits.
#' @export
#'
#' @examples \dontrun{
#' ct_register_token("some_valid_token_str")
#' }
ct_register_token <- function(token) {
  # input validation.
  stopifnot(is.character(token) || is.null(token))

  # Get number of queries left for the current hour.
  queries_this_hour <- get("queries_this_hour", envir = ct_env)

  # If token is NULL, reset all options to pkg load defaults.
  if (is.null(token)) {
    ct_options <- list(
      comtrade = list(
        token = NULL,
        account_type = "standard",
        per_hour_limit = 100,
        per_second_limit = 1
      )
    )
    class(ct_options) <- "comtradr_credentials"
    options(comtradr = ct_options)

    # Reset "queries_this_hour" to 100 if it's currently greater than 100.
    if (queries_this_hour > 100) {
      assign("queries_this_hour", 100, envir = ct_env)
    }

    return(invisible())
  }

  # Set token within options.
  ct_options <- getOption("comtradr")
  ct_options$comtrade$token <- token
  ct_options$comtrade$account_type <- "premium"
  ct_options$comtrade$per_hour_limit <- 10000
  class(ct_options) <- "comtradr_credentials"
  options(comtradr = ct_options)

  # Change the hourly limit within the env ct_env. Subtract the number of
  # queries already performed this hour from the update value.
  curr_account_type <- getOption("comtradr")$comtrade$account_type
  if (curr_account_type == "standard") {
    new_hr_limit <- 10000 - (100 - queries_this_hour)
  } else if (curr_account_type == "premium") {
    new_hr_limit <- 10000 - (10000 - queries_this_hour)
  }
  assign("queries_this_hour", new_hr_limit, envir = ct_env)

  return(invisible())
}
