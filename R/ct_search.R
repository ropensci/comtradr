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
#'  "all". If inputing a date, must be string w/ structure "yyyy-mm-dd".
#' @param end_date End date of a time period, or "all". Default value is "all".
#'  If inputing a date, must be string w/ structure "yyyy-mm-dd".
#' @param commod_codes Character vector of commodity codes, or "TOTAL". Valid
#'  commodity codes as input will restrict the query to only look for trade
#'  related to those commodities, "TOTAL" as input will return all trade
#'  between the indicated reporter country(s) and partner country(s). Default
#'  value is "TOTAL".
#' @param max_rec Max number of records returned from each API call, as an
#'  integer. If max_rec is set to NULL, then value is determined by whether or
#'  not an API token has been rergistered. API cap without a token is 50000,
#'  cap with a valid token is 250000. Default value is NULL. For details on
#'  how to register a valid token, see \code{\link{ct_register_token}}.
#' @param type Type of trade, as a character string. Must be either "goods" or
#'  "services". Default value is "goods".
#' @param url Base of the Comtrade url string, as a character string. Default
#'  value is "https://comtrade.un.org/api/get?" and should mot be changed
#'  unless Comtrade changes their endpoint url.
#' @param col_name Should the output data frame have human-friendly or
#'  machine-friendly column names. Human-friendly means easy for a human to
#'  read and understand. Machine-friendly means easy for a machine to parse,
#'  and may not be intuitive for a human to read and understand. Must be
#'  either "human" or "machine". Default value is "human".
#'
#' @details Basic rate limit restrictions. For details on how to register a
#'  valid token, see \code{\link{ct_register_token}}. For API docs on rate
#'  limits, see \url{https://comtrade.un.org/data/doc/api/#Limits}
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
#'    for each is five (for date range, if not using "all", then the
#'    "start_date" and "end_date" must at most span five months or five years).
#'  \item For param "commod_codes", if not using input "All", then the maximum
#'    number of input values is 20 (although "All" is always a valid input).
#'  }
#'
#'  This function returns objects with metadata related to the API call that
#'  can be accessed via \code{\link{attributes}}. The metadata accessable is:
#'  \itemize{
#'  \item url: url of the API call.
#'  \item time_stamp: date-time of the API call.
#'  \item req_duration: total duration of the API call, in seconds.
#'  }
#'
#' @return Data frame of Comtrade shipping data.
#'
#' @export
#'
#' @importFrom magrittr "%>%"
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
#' shrimp_codes <- commodity_lookup("shrimp",
#'                                  return_code = TRUE,
#'                                  return_char = TRUE)
#'
#' # Make API call
#' ex_2 <- ct_search(reporters = "Canada",
#'                   partners = "All",
#'                   trade_direction = "all",
#'                   start_date = "2011-01-01",
#'                   end_date = "2015-01-01",
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
                      url = "https://comtrade.un.org/api/get?",
                      col_name = c("human", "machine")) {

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
    stop(msg)
  }

  # Transformations to type.
  type <- match.arg(type)
  if (type == "goods") {
    type <- "C"
  } else if (type == "services") {
    type <- "S"
  }

  # Transformations to freq.
  freq <- match.arg(freq)
  if (freq == "annual") {
    freq <- "A"
  } else if (freq == "monthly") {
    freq <- "M"
  }

  # Transformations to start_date and end_date.
  if (any(c(start_date, end_date) %in% c("all", "All", "ALL"))) {
    date_range <- "all"
  } else {
    sd <- tryCatch(
      as.Date(start_date, format = "%Y-%m-%d"), error = function(e) e
    )
    ed <- tryCatch(
      as.Date(end_date, format = "%Y-%m-%d"), error = function(e) e
    )
    if (any(methods::is(sd, "error"), methods::is(ed, "error"),
            is.na(sd), is.na(ed))) {
      stop("args 'start_date' & 'end_date' must either be 'all' or be dates ",
           "that have format 'yyyy-mm-dd'", call. = FALSE)
    }

    if (freq == "A") {
      date_range <- seq.Date(as.Date(start_date, format = "%Y-%m-%d"),
                             as.Date(end_date, format = "%Y-%m-%d"),
                             by = "year") %>%
        as.Date() %>%
        format(format = "%Y")
    } else if (freq == "M") {
      date_range <- seq.Date(as.Date(start_date, format = "%Y-%m-%d"),
                             as.Date(end_date, format = "%Y-%m-%d"),
                             by = "month") %>%
        as.Date() %>%
        format(format = "%Y%m")
    }

    # Check to make sure the total date range is five or fewer months/years.
    if (length(date_range) > 5) {
      stop(paste("if date range specified, the span of months or years",
                 "must be five or fewer"))
    }

    date_range <- paste(date_range, collapse = ",")
  }

  # Transformations to reporters.
  if (any(reporters %in% c("all", "All", "ALL"))) {
    reporters <- "All"
  }

  if (!all(reporters %in% country_df$`country name`)) {
    err <- paste(
      reporters[!reporters %in% country_df$`country name`],
      collapse = ", "
    )
    stop(paste("From arg 'reporters', these values were not found in the",
               "country database:", err))
  }

  if (length(reporters) > 5) {
    stop(paste("arg 'reporters' must be 'all' or a char vector of country",
               "names, length five or fewer"))
  }

  reporters <- purrr::map_chr(reporters, function(x) {
    country_df[country_df$`country name` == x &
                   country_df$type == "reporter", ]$code
  }) %>%
    paste(collapse = ",")

  # Transformations to partners.
  if (any(partners %in% c("all", "All", "ALL"))) {
    partners <- "All"
  }

  if (!all(partners %in% country_df$`country name`)) {
    err <- paste(
      partners[!partners %in% country_df$`country name`],
      collapse = ", "
    )
    stop(paste("From arg 'partners', these values were not found in the",
               "country database:", err))
  }

  if (length(partners) > 5) {
    stop(paste("arg 'partners' must be 'all' or a char vector of country",
               "names, length five or fewer"))
  }

  partners <- purrr::map_chr(partners, function(x) {
    country_df[country_df$`country name` == x &
                   country_df$type == "partner", ]$code
  }) %>%
    paste(collapse = ",")

  # Transformations to trade_direction.
  rg <- vector()

  if (any(trade_direction %in% c("all", "All", "ALL"))) {
    rg <- "all"
  } else {
    trade_direction <- match.arg(trade_direction, several.ok = TRUE)

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

  # Transformations to commod_codes.
  stopifnot(is.character(commod_codes))
  if (any(tolower(commod_codes) == "total")) {
    commod_codes <- "TOTAL"
  } else if (any(tolower(commod_codes) == "all")) {
    commod_codes <- "ALL"
  } else if (length(commod_codes) > 20) {
    stop(paste("arg 'commod_codes' must be 'all' or a char vector of",
               "commodity codes, length 20 or fewer"))
  } else if (length(commod_codes) > 1) {
    commod_codes <- paste(commod_codes, collapse = ",")
  }

  # Transformations to colname:
  col_name <- match.arg(col_name)
  if (col_name == "human") {
    col_name <- "H"
  } else if (col_name == "machine") {
    col_name <- "M"
  }

  # Get the commodity code scheme type to use.
  code_type <- ct_commodity_db_type()

  # Get max_rec. If arg value is set to NULL, then max_rec is determined by
  # whether an API token has been registered. If a token has been registered,
  # then max_rec will be set to 250000, otherwise it will be set to 50000.
  if (is.null(max_rec)) {
    if (is.null(token)) {
      max_rec <- 50000
    } else {
      max_rec <- 250000
    }
  } else {
    max_rec <- as.numeric(max_rec)
  }

  # Stitch together the url of the API call.
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
    "&head=", col_name
  )

  # If token within global options is not NULL, append the token str to the
  # end of the API url.
  if (!is.null(token)) {
    url <- paste0(url, "&token=", token)
  }

  # Time stamp the current api query.
  assign("last_query", Sys.time(), envir = ct_env)

  # Execute API call.
  res <- execute_api_request(url, col_name)

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
#' @param url str, url to send to the Comtrade API.
#' @param col_name str, indicating whether to edit the col headers of the
#'  return data frame.
#'
#' @return data frame of API return data.
execute_api_request <- function(url, col_name) {
  # Ping API.
  res <- httr::GET(url, httr::user_agent(get("ua", envir = ct_env)))

  # Check status code of res (if 400 or greater, throw an error).
  if (httr::http_error(res)) {
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
        "API did not return json. Instead, %s was returned",
        httr::http_type(res)
      ), call. = FALSE
    )
  }

  # Get response content and data from res.
  raw_data <- res %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)

  # If no data returned and Comtrade provided a useful message indicating why,
  # throw error that uses the useful message.
  if (length(raw_data$dataset) == 0 &&
      !is.null(raw_data$validation$message)) {
    stop(
      sprintf(
        "Comtrade API request failed, with status code [%s]\nFail Reason: %s",
        httr::status_code(res),
        raw_data$validation$message
      ), call. = FALSE
    )
  }

  # If arg "col_name" == "H" (for "human"), edit col headers of the return
  # data frame.
  if (col_name == "H") {
    colnames(raw_data$dataset) <- api_col_names()
  }
  return(raw_data$dataset)
}

