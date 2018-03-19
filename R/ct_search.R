#' Get UN Comtrade data
#'
#' Make queries to the UN Comtrade API, data is returned as a tidy data frame.
#' Comtrade is a DB hosted by the United Nations that houses country-level
#' shipping data. Full API docs can be found here:
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param reporters Country(s) of interest, as a character vector. Can either
#'  be a vector of country names, or "All" to represent all countries.
#' @param partners Country(s) that have interacted with the reporter
#'  country(s), as a character vector. Can either be a vector of country names,
#'  or "All" to represent all countries.
#' @param trade_direction Indication of which trade directions on which to
#'  focus, as a character vector. Must either be "all", or a vector containing
#'  any combination of the following: "imports", "exports", "re_imports",
#'  "re_exports". Default value is "all".
#' @param freq Time frequency of the returned results, as a character string.
#'  Must be either "annual" or "monthly". Default value is "annual".
#' @param start_date Start date of a time period, or "all". Default value is
#'  "all". See "details" for more info on valid input formats when not using
#'  "all" as input.
#' @param end_date End date of a time period, or "all". Default value is
#'  "all". See "details" for more info on valid input formats when not using
#'  "all" as input.
#' @param commod_codes Character vector of commodity codes, or "TOTAL". Valid
#'  commodity codes as input will restrict the query to only look for trade
#'  related to those commodities, "TOTAL" as input will return all trade
#'  between the indicated reporter country(s) and partner country(s). Default
#'  value is "TOTAL".
#' @param max_rec Max number of records returned from each API call, as an
#'  integer. If max_rec is set to NULL, then value is determined by whether or
#'  not an API token has been registered. API cap without a token is 50000,
#'  cap with a valid token is 250000. Default value is NULL. For details on
#'  how to register a valid token, see \code{\link{ct_register_token}}.
#' @param type Type of trade, as a character string. Must be either "goods" or
#'  "services". Default value is "goods".
#' @param url Base of the Comtrade url string, as a character string. Default
#'  value is "https://comtrade.un.org/api/get?" and should mot be changed
#'  unless Comtrade changes their endpoint url.
#'
#' @details Basic rate limit restrictions listed below. For details on how to
#'  register a valid token, see \code{\link{ct_register_token}}. For API docs
#'  on rate limits, see \url{https://comtrade.un.org/data/doc/api/#Limits}
#'  \itemize{
#'  \item Without authentication token: 1 request per second, 100 requests
#'    per hour (each per IP address).
#'  \item With valid authentication token: 1 request per second, 10,000
#'    requests per hour (each per IP address or authenticated user).
#'  }
#'
#'  In addition to these rate limits, the API imposes some limits on
#'  parameter combinations, they are listed below:
#'  \itemize{
#'  \item Between params "reporters", "partners", and the query date range (as
#'    dictated by the two params "start_date" and "end_date"), only one of
#'    these three may use the catch-all input "All".
#'  \item For the same group of three ("reporters", "partners", date range),
#'    if the input is not "All", then the maximum number of input values
#'    for each is five. For date range, if not using "All", then the
#'    "start_date" and "end_date" must not span more than five months or five
#'    years. There is one exception to this rule, if arg "freq" is "monthly",
#'    then a single year can be passed to "start_date" and "end_date" and the
#'    API will return all of the monthly data for that year.
#'  \item For param "commod_codes", if not using input "All", then the maximum
#'    number of input values is 20 (although "All" is always a valid input).
#'  }
#'
#'  This function returns objects with metadata related to the API call that
#'  can be accessed via \code{\link{attributes}}. The metadata accessible is:
#'  \itemize{
#'  \item url: url of the API call.
#'  \item time_stamp: date-time of the API call.
#'  \item req_duration: total duration of the API call, in seconds.
#'  }
#'
#'  For args \code{start_date} and \code{end_date}, if inputing a date (as
#'  opposed to the catch-all input "all"), valid input format is dependent on
#'  the input passed to arg \code{freq}. If \code{freq} is "annual",
#'  \code{start_date} and \code{end_date} must be either a string w/ format
#'  "yyyy" or "yyyy-mm-dd", or a year as an integer (so "2016", "2016-01-01",
#'  and 2016 would all be valid). If \code{freq} is "monhtly",
#'  \code{start_date} and \code{end_date} must be a string with format
#'  "yyyy-mm" or "yyyy-mm-dd" (so "2016-02" and "2016-02-01" would both be
#'  valid).
#'
#' @return Data frame of Comtrade shipping data.
#'
#' @export
#'
#' @examples \dontrun{
#' ## Example API call number 1:
#' # All exports from China to South Korea, United States and Mexico over all
#' # years.
#' ex_1 <- ct_search(reporters = "China",
#'                   partners = c("Rep. of Korea", "USA", "Mexico"),
#'                   trade_direction = "exports")
#' nrow(ex_1)
#'
#' ## Example API call number 2:
#' # All shipments related to shrimp between Canada and all other countries,
#' # between 2011 and 2015.
#' # Perform "shrimp" query
#' shrimp_codes <- ct_commodity_lookup("shrimp",
#'                                     return_code = TRUE,
#'                                     return_char = TRUE)
#'
#' # Make API call
#' ex_2 <- ct_search(reporters = "Canada",
#'                   partners = "All",
#'                   trade_direction = "all",
#'                   start_date = 2011,
#'                   end_date = 2015,
#'                   commod_codes = shrimp_codes)
#' nrow(ex_2)
#'
#' # Access metadata
#' attributes(ex_2)$url
#' attributes(ex_2)$time_stamp
#' attributes(ex_2)$req_duration
#' }
ct_search <- function(reporters, partners,
                      trade_direction = c("all", "imports", "exports",
                                          "re_imports", "re_exports"),
                      freq = c("annual", "monthly"),
                      start_date = "all", end_date = "all",
                      commod_codes = "TOTAL", max_rec = NULL,
                      type = c("goods", "services"),
                      url = "https://comtrade.un.org/api/get?") {

  ## Input validation related to API limits on parameter combinations.

  # Between params "reporters", "partners", and the query date range (as
  # dictated by the two params "start_date" and "end_date"), only one of
  # these three may use the catch-all input "All".
  if (
    all(
      any(tolower(reporters) == "all"),
      any(tolower(partners) == "all"),
      any(tolower(c(start_date, end_date)) == "all")
    )
  ) {
    stop(paste("between 'reporters', 'partners', and date range, only one",
               "of these may be 'all'"), call. = FALSE)
  }

  # Fetch current values within ct_env (these values help manage
  # throttling of API queries).
  cache_vals <- get_cache_values()

  # If last api query was less than 2 seconds ago, delay code by 2 seconds.
  if (Sys.time() < cache_vals$last_query + 2) {
    Sys.sleep(2)
  }

  # Fetch current value of user token, to see if an auth token has been
  # registered.
  token <- getOption("comtradr")$comtrade$token

  # Fetch the country database from ct_env.
  country_df <- get_country_db()

  # Check to see if the current one hour time limit needs to be reset. If
  # current value is NULL, initialize the cache value with the current time.
  if (is.null(cache_vals$next_hour_reset) ||
      Sys.time() > ct_get_reset_time()) {
    assign("next_hour_reset", Sys.time(), envir = ct_env)
  }

  # Check to make sure the hourly query limit hasn't been reached.
  if (cache_vals$queries_this_hour == 0) {
    msg <- paste("over the hourly limit. hour resets at",
                 ct_get_reset_time())
    stop(msg, call. = FALSE)
  }

  ## Get the commodity code scheme type to use.
  code_type <- ct_commodity_db_type()

  ## Transformations to type.
  type <- match.arg(type)

  if (type == "goods") {
    type <- "C"
  } else if (type == "services") {
    type <- "S"
  }

  # If type == "S", then code_type must be "EB02".
  if (type == "S" && code_type != "EB02") {
    code_type <- "EB02"
  }

  ## Transformations to freq.
  freq <- match.arg(freq)
  if (freq == "annual") {
    freq <- "A"
  } else if (freq == "monthly") {
    freq <- "M"
  }

  ## Transformations to start_date and end_date.
  if (any(c(start_date, end_date) %in% c("all", "All", "ALL"))) {
    date_range <- "all"
  } else {
    date_range <- get_date_range(start_date, end_date, freq)
  }

  ## Transformations to reporters.
  if (any(reporters %in% c("all", "All", "ALL"))) {
    reporters <- "All"
  }

  if (!all(reporters %in% country_df$country_name)) {
    err <- paste(
      reporters[!reporters %in% country_df$country_name],
      collapse = ", "
    )
    stop(paste("From arg 'reporters', these values were not found in the",
               "country database:", err), call. = FALSE)
  }

  if (length(reporters) > 5) {
    stop(paste("arg 'reporters' must be 'all' or a char vector of country",
               "names, length five or fewer"), call. = FALSE)
  }

  reporters <- purrr::map_chr(reporters, function(x) {
    country_df[country_df$country_name == x &
                   country_df$reporter == TRUE, ]$code
  }) %>%
    paste(collapse = ",")

  ## Transformations to partners.
  if (any(partners %in% c("all", "All", "ALL"))) {
    partners <- "All"
  }

  if (!all(partners %in% country_df$country_name)) {
    err <- paste(
      partners[!partners %in% country_df$country_name],
      collapse = ", "
    )
    stop(paste("From arg 'partners', these values were not found in the",
               "country database:", err), call. = FALSE)
  }

  if (length(partners) > 5) {
    stop(paste("arg 'partners' must be 'all' or a char vector of country",
               "names, length five or fewer"), call. = FALSE)
  }

  partners <- purrr::map_chr(partners, function(x) {
    country_df[country_df$country_name == x &
                   country_df$partner == TRUE, ]$code
  }) %>%
    paste(collapse = ",")

  ## Transformations to trade_direction.
  if (any(tolower(trade_direction) == "all")) {
    trade_direction <- "all"
  } else {
    trade_direction <- match.arg(trade_direction, several.ok = TRUE)
    rg <- vector()

    if (any(trade_direction == "imports")) {
      rg <- "1"
    }

    if (any(trade_direction == "exports")) {
      if (length(rg) == 0) {
        rg <- "2"
      } else {
        rg <- paste(rg, "2", sep = ",")
      }
    }

    if (any(trade_direction == "re_exports")) {
      if (length(rg) == 0) {
        rg <- "3"
      } else {
        rg <- paste(rg, "3", sep = ",")
      }
    }

    if (any(trade_direction == "re_imports")) {
      if (length(rg) == 0) {
        rg <- "4"
      } else {
        rg <- paste(rg, "4", sep = ",")
      }
    }

    trade_direction <- rg
  }

  ## Transformations to commod_codes.
  stopifnot(is.character(commod_codes))
  if (any(tolower(commod_codes) == "total")) {
    if (code_type != "EB02") {
      commod_codes <- "TOTAL"
    } else {
      # If code_type is "EB02" and input to commod_codes includes "total",
      # then make commod_codes "200" ("total == "200" in EB02).
      commod_codes <- "200"
    }
  } else if (any(tolower(commod_codes) == "all")) {
    commod_codes <- "ALL"
  } else if (length(commod_codes) > 20) {
    stop(paste("arg 'commod_codes' must be 'all' or a char vector of",
               "commodity codes, length 20 or fewer"), call. = FALSE)
  } else if (length(commod_codes) > 1) {
    commod_codes <- paste(commod_codes, collapse = ",")
  }

  ## Get max_rec. If arg value is set to NULL, then max_rec is determined by
  ## whether an API token has been registered. If a token has been registered,
  ## then max_rec will be set to 250000, otherwise it will be set to 50000.
  if (is.null(max_rec)) {
    if (is.null(token)) {
      max_rec <- 50000
    } else {
      max_rec <- 250000
    }
  } else {
    max_rec <- as.numeric(max_rec)
  }

  ## Stitch together the url of the API call.
  url <- paste0(
    url,
    "max=", max_rec,
    "&type=", type,
    "&freq=", freq,
    "&px=", code_type,
    "&ps=", date_range,
    "&r=", reporters,
    "&p=", partners,
    "&rg=", trade_direction,
    "&cc=", commod_codes,
    "&fmt=", "json",
    "&head=", "H"
  )

  ## If token within global options is not NULL, append the token str to the
  ## end of the API url.
  if (!is.null(token)) {
    url <- paste0(url, "&token=", token)
  }

  # Time stamp the current api query.
  assign("last_query", Sys.time(), envir = ct_env)

  # Execute API call.
  res <- execute_api_request(url)

  # Edit cache variable "queries_this_hour" to be one less.
  assign("queries_this_hour", (cache_vals$queries_this_hour - 1),
         envir = ct_env)

  # Assign metadata attributes to obj "res".
  attributes(res)$url <- url
  last_query <- get("last_query", envir = ct_env)
  attributes(res)$time_stamp <- last_query
  attributes(res)$req_duration <- as.double(
    difftime(Sys.time(), last_query, units = "secs")
  )

  return(res)
}


#' Send API request to Comtrade.
#'
#' @param url char str, url to send to the Comtrade API.
#'
#' @noRd
#' @return data frame of API return data.
execute_api_request <- function(url) {
  # Ping API.
  res <- httr::GET(url, httr::user_agent(get("ua", envir = ct_env)))

  # Check status code of res (if not 200, throw an error).
  if (httr::status_code(res) != 200) {
    stop(
      sprintf(
        "Comtrade API request failed, with status code [%s]\n%s",
        httr::status_code(res)
      ), call. = FALSE
    )
  }

  # Check content type of res (if not json, throw an error).
  if (httr::http_type(res) != "application/json") {
    stop(
      sprintf(
        "API did not return json. Instead, %s data was returned",
        httr::http_type(res)
      ), call. = FALSE
    )
  }

  # Get response content and data from res.
  raw_data <- res %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  df <- raw_data$dataset

  # Check length of return data.
  if (length(df) == 0) {
    if (!is.null(raw_data$validation$message)) {
      # If no data returned and Comtrade provided a useful message indicating
      # why, throw error that uses the useful message.
      stop(
        sprintf(
          "API request failed. Err msg from Comtrade:\n  %s",
          raw_data$validation$message
        ), call. = FALSE
      )
    } else {
      # If no data returned and there's no Comtrade message explaining why,
      # this is an indication that there was no error and there really is no
      # data to return (based on the input values given). Prep an empty
      # data frame to return.
      df <- matrix(ncol = 35, nrow = 0) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        `colnames<-`(names(cols))
    }
  }

  # Within the return data frame, replace all empty strings with NA, and
  # rename the column headers.
  if (nrow(df) > 0) {
    is.na(df) <- df == ""
    if (all(colnames(df) %in% cols)) {
      colnames(df) <- purrr::map_chr(colnames(df), function(x) {
        names(cols)[which(cols == x)]
      })
    } else {
      warning(paste("col headers of the return data from the API cannot be",
                    "mapped to package data. Data will be returned with the",
                    "Comtrade API col headers."), call. = FALSE)
    }
  }

  return(df)
}


#' Get Date Range
#'
#' @return Date range as a single string, comma sep.
#' @noRd
get_date_range <- function(start_date, end_date, freq) {
  start_date <- as.character(start_date)
  end_date <- as.character(end_date)

  if (freq == "A") {
    # Date range when freq is "annual" (date range by year).
    start_date <- convert_to_date(start_date)
    end_date <- convert_to_date(end_date)
    date_range <- seq.Date(start_date, end_date, by = "year") %>%
      format(format = "%Y")
  } else if (freq == "M") {
    # Date range when freq is "monthly".
    sd_year <- is_year(start_date)
    ed_year <- is_year(end_date)
    if (sd_year && ed_year) {
      # If start_date and end_date are both years ("yyyy") and are identical,
      # return the single year as the date range.
      if (identical(start_date, end_date)) {
        return(start_date)
      } else {
        stop("Cannot get more than a single year's worth of monthly data ",
             "in a single query", call. = FALSE)
      }
    } else if (!sd_year && !ed_year) {
      # If neither start_date nor end_date are years, get date range by month.
      start_date <- convert_to_date(start_date)
      end_date <- convert_to_date(end_date)
      date_range <- seq.Date(start_date, end_date, by = "month") %>%
        format(format = "%Y%m")
    } else {
      # Between start_date and end_date, if one is a year and the other isn't,
      # throw an error.
      stop("If arg 'freq' is 'monhtly', 'start_date' and 'end_date' must ",
           "have the same format", call. = FALSE)
    }
  }

  # If the derived date range is longer than five elements, throw an error.
  if (length(date_range) > 5) {
    stop("If specifying years/months, cannot search more than five ",
         "consecutive years/months in a single query", call. = FALSE)
  }

  return(paste(date_range, collapse = ","))
}


#' Given a numeric or character date, convert to an object with class "Date".
#'
#' @return Object of class "Date" (using base::as.Date()).
#' @noRd
convert_to_date <- function(date_obj) {
  # Convert to char.
  #date_obj <- as.character(date_obj)
  # Convert to Date.
  if (is_year(date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01-01"), format = "%Y-%m-%d")
  } else if (is_year_month(date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01"), format = "%Y-%m-%d")
  } else {
    date_obj <- as.Date(date_obj, format = "%Y-%m-%d")
  }
  # If conversion to Date failed, throw error.
  if (is.na(date_obj)) {
    stop(sprintf(
      paste("arg '%s' must be a date with one of these formats:\n",
            "int: yyyy\n",
            "char: 'yyyy'\n",
            "char: 'yyyy-mm'\n",
            "char: 'yyyy-mm-dd'"),
      deparse(substitute(date_obj))
    ), call. = FALSE)
  }

  date_obj
}


#' Is input a year string or not.
#'
#' @noRd
is_year <- function(x) {
  grepl("^\\d{4}$", x)
}


#' Is input a year-month string or not.
#'
#' @noRd
is_year_month <- function(x) {
  grepl("^\\d{4}-\\d{2}", x)
}
