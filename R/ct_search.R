#' Get UN Comtrade data via API
#'
#' Make queries to the UN Comtrade API, data is returned as a tidy dataframe.
#' Comtrade is a DB hosted by the United Nations that houses country-level
#' shipping data. Full API docs can be found here:
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param reporters Country(s) of interest, as a character vector. Can either
#'  be a vector of country names, or "All" to represent all countries.
#' @param partners Country(s) that have interacted with the reporter
#'  country(s), as a character vector. Can either be a vector of country names,
#'  or "All" to represent all countries.
#' @param countrytable Dataframe of country names and associated country codes
#'  that work within the Comtrade API calls. Includes both reporters & partners.
#' @param url Base of the Comtrade url string, as a character string.
#' @param maxrec Max number of records returned from each API call, as an
#'  integer. API cap without a token is 50000, cap with a valid token is
#'  250000. Default value is 50000.
#' @param type Type of trade, as a character string. Must be either "goods" or
#'  "services".
#' @param freq Time frequency of the returned results, as a character string.
#'  Must be either "annual" or "monthly".
#' @param startdate Start date of a time period, or "all". Default value is
#'  "all". If inputing a date, must be string w/ structure "yyyy-mm-dd".
#' @param enddate End date of a time period, or "all". Default value is "all".
#'  If inputing a date, must be string w/ structure "yyyy-mm-dd".
#' @param tradedirection Indication of which trade directions on which to
#'  focus, as a character vector. Must either be "all", or a vector containing
#'  any combination of the following: "imports", "exports", "re-imports",
#'  "re-exports".
#' @param commodcodes Character vector of commodity codes, or "TOTAL". Valid
#'  commodity codes as input will restrict the query to only look for trade
#'  related to those commodities, "TOTAL" as input will return all trade
#'  between the indicated reporter country(s) and partner country(s). Default
#'  value is "TOTAL".
#' @param fmt Indication as to the format of the returned data, as a character
#'  string. Must be either "json" or "csv". Regardless of the fmt used, the
#'  return data will be in the form of a tidy dataframe. "json" is the
#'  suggested value for this parameter, as the API tends to provide more
#'  detailed feedback on why a query failed when using json.
#' @param colname Should the output dataframe have human-friendly or
#'  machine-friendly column names. Human-friendly means easy for a human to
#'  read and understand, and may contain special characters and spaces.
#'  Machine-friendly means easy for a machine to parse, and may not contain
#'  special characters or spaces. Must be either "human" or "machine". Default
#'  value is "human".
#' @param token Authorization token, as a character string. Default value is
#'  NULL.
#' @param codetype Trade data classification scheme to use, as a character
#'  string. See "Details" for a list of a valid inputs.
#'
#' @details Basic rate limit restrictions. For full details see
#'  \url{https://comtrade.un.org/data/doc/api/#Limits}
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
#'    dictated by the two params "startdate" and "enddate"), only one of these
#'    three may use the catch-all input "All".
#'  \item For the same group of three ("reporters", "partners", date range),
#'    if the input is not "All", then the maximum number of input values
#'    for each is five (for date range, if not using "all", then the
#'    "startdate" and "enddate" must at most span five months or five years).
#'  \item For param "commodcodes", if not using input "All", then the maximum
#'    number of input values is 20 (although "All" is always a valid input).
#'  }
#'
#'  The default for param \code{codetype} is \code{HS}. Below is a list of all
#'  valid inputs with a very brief description for each. For more information
#'  on each of these types, see
#'  \url{https://comtrade.un.org/data/doc/api/#DataAvailabilityRequests}
#'  \itemize{
#'  \item \code{HS}: Harmonized System (HS), as reported
#'  \item \code{HS1992}: HS 1992
#'  \item \code{HS1996}: HS 1996
#'  \item \code{HS2002}: HS 2002
#'  \item \code{HS2007}: HS 2007
#'  \item \code{HS2012}: HS 2012
#'  \item \code{SITC}: Standard International Trade Classification (SITC), as
#'    reported
#'  \item \code{SITCrev1}: SITC Revision 1
#'  \item \code{SITCrev2}: SITC Revision 2
#'  \item \code{SITCrev3}: SITC Revision 3
#'  \item \code{SITCrev4}: SITC Revision 4
#'  \item \code{BEC}: Broad Economic Categories
#'  \item \code{EB02}: Extended Balance of Payments Services Classification
#'  }
#'
#' @return List of length three, elements are:
#'  \itemize{
#'  \item \code{msg}: Brief message on success/failure of the API call.
#'  \item \code{details}: More detailed message on success/failure of the API
#'    call.
#'  \item \code{data}: Dataframe object of return data.
#'  }
#' @export
#'
#' @examples \dontrun{
#' # Create the country lookup table
#' countrydf <- ct_countries_table()
#'
#' ## Example API call number 1:
#' # All exports from China to South Korea, United States and Mexico over all
#' # years.
#' comtrade <- ct_search(reporters = "China",
#'                       partners = c("Rep. of Korea", "USA", "Mexico"),
#'                       countrytable = countrydf,
#'                       tradedirection = "exports")
#' comtrade$msg
#' [1] "Data returned"
#' comtrade$details
#' [1] "Connection successful"
#' nrow(comtrade$data)
#' [1] 75
#'
#' ## Example API call number 2:
#' # All shipments related to halibut between Canada and all other countries,
#' # between 2011 and 2015.
#' # Create the commodities lookup table
#' commoditydf <- ct_commodities_table("HS")
#'
#' # Perform "shrimp" query
#' shrimp_codes <- commodity_lookup("shrimp",
#'                                  commoditydf,
#'                                  return_code = TRUE,
#'                                  return_char = TRUE,
#'                                  verbose = TRUE)
#'
#' # Make API call
#' shrimp_codes <- commodity_lookup("shrimp",
#'                                  commoditydf,
#'                                  return_code = TRUE,
#'                                  return_char = TRUE,
#'                                  verbose = TRUE)
#' comtrade <- ct_search(reporters = "Canada",
#'                       partners = "All",
#'                       countrytable = countrydf,
#'                       tradedirection = "all",
#'                       startdate = "2011-01-01",
#'                       enddate = "2015-01-01",
#'                       commodcodes = shrimp_codes)
#' comtrade$msg
#' [1] "Data returned"
#' comtrade$details
#' [1] "Connection successful"
#' nrow(comtrade$data)
#' [1] 1321
#' }
ct_search <- function(reporters, partners, countrytable,
                      url = "https://comtrade.un.org/api/get?", maxrec = 50000,
                      type = c("goods", "services"),
                      freq = c("annual", "monthly"),
                      startdate = "all", enddate = "all",
                      tradedirection = c("all", "imports", "exports",
                                         "re-imports", "re-exports"),
                      commodcodes = "TOTAL", fmt = c("json", "csv"),
                      colname = c("human", "machine"), token = NULL,
                      codetype = c("HS", "H0", "H1", "H2", "H3", "H4",
                                   "ST", "S1", "S2", "S3", "S4",
                                   "BEC", "EB02")) {

  # Transformations to type:
  type <- match.arg(type)
  if (type == "goods") {
    type <- "C"
  } else if (type == "services") {
    type <- "S"
  }

  # Transformations to freq:
  freq <- match.arg(freq)
  if (freq == "annual") {
    freq <- "A"
  } else if (freq == "monthly") {
    freq <- "M"
  }

  # Transformations to startdate and enddate:
  if (any(c(startdate, enddate) %in% c("all", "All", "ALL"))) {
    daterange <- "all"
  } else {
    sd <- tryCatch(
      as.Date(startdate, format = "%Y-%m-%d"), error = function(e) e
    )
    ed <- tryCatch(
      as.Date(enddate, format = "%Y-%m-%d"), error = function(e) e
    )
    if (any(methods::is(sd, "error"), methods::is(ed, "error"),
            is.na(sd), is.na(ed))) {
      stop("params 'startdate' & 'enddate' must either be 'all' or be dates ",
           "that have format 'yyyy-mm-dd'", call. = FALSE)
    }

    if (freq == "A") {
      daterange <- seq.Date(as.Date(startdate, format = "%Y-%m-%d"),
                            as.Date(enddate, format = "%Y-%m-%d"),
                            by = "year") %>%
        as.Date() %>%
        format(format = "%Y") %>%
        paste(collapse = ",")
    } else if (freq == "M") {
      daterange <- seq.Date(as.Date(startdate, format = "%Y-%m-%d"),
                            as.Date(enddate, format = "%Y-%m-%d"),
                            by = "month") %>%
        as.Date() %>%
        format(format = "%Y%m") %>%
        paste(collapse = ",")
    }
  }

  # Transformations to reporters:
  if (any(reporters %in% c("all", "All", "ALL"))) {
    reporters <- "All"
  }

  ids <- sapply(
    reporters, function(x)
      countrytable[which(countrytable$`country name` == x &
                           countrytable$type == "reporter"), ]$code,
    USE.NAMES = FALSE)

  if (is.character(ids)) {
    reporters <- paste(ids, collapse = ",")
  } else if (is.list(ids)) {
    err <- paste(
      reporters[!reporters %in% names(unlist(ids))], collapse = ", ")
    stop(paste("From param reporters, these values were not found in the ",
               "country code lookup table:", err))
  }

  # Transformations to partners:
  if (any(partners %in% c("all", "All", "ALL"))) {
    partners <- "All"
  }

  ids <- sapply(
    partners, function(x)
      countrytable[which(countrytable$`country name` == x &
                           countrytable$type == "partner"), ]$code,
    USE.NAMES = FALSE)

  if (is.character(ids)) {
    partners <- paste(ids, collapse = ",")
  } else if (is.list(ids)) {
    err <- paste(
      partners[!partners %in% names(unlist(ids))], collapse = ", ")
    stop(paste("From param partners, these values were not found in the ",
               "country code lookup table:", err))
  }

  # Transformations to tradedirection:
  rg <- vector()

  if (any(tradedirection %in% c("all", "All", "ALL"))) {
    rg <- "all"
  } else {
    tradedirection <- match.arg(tradedirection, several.ok = TRUE)

    if (any(tradedirection == "imports")) {
      rg <- "1"
    }

    if (any(tradedirection == "exports")) {
      if (length(rg) == 0) {
        rg <- "2"
      } else {
        rg <- paste(rg, "2", sep = ",")
      }
    }

    if (any(tradedirection == "re-exports")) {
      if (length(rg) == 0) {
        rg <- "3"
      } else {
        rg <- paste(rg, "3", sep = ",")
      }
    }

    if (any(tradedirection == "re-imports")) {
      if (length(rg) == 0) {
        rg <- "4"
      } else {
        rg <- paste(rg, "4", sep = ",")
      }
    }

    tradedirection <- rg
  }

  # Transformations to commodcodes:
  if (any(commodcodes %in% c("TOTAL", "Total", "total"))) {
    commodcodes <- "TOTAL"
  } else if (any(commodcodes %in% c("ALL", "All", "all"))) {
    commodcodes <- "ALL"
  } else if (length(commodcodes) > 1) {
    commodcodes <- paste(commodcodes, collapse = ",")
  }

  # Transformations to fmt:
  fmt <- match.arg(fmt)

  # Transformations to colname:
  colname <- match.arg(colname)
  if (colname == "human") {
    colname <- "H"
  } else if (colname == "machine") {
    colname <- "M"
  }

  # Transformations to codetype:
  codetype <- match.arg(codetype)

  # Stitch together the url of the API call.
  url <- paste0(url,
                "max=", maxrec,
                "&type=", type,
                "&freq=", freq,
                "&px=", codetype,
                "&ps=", daterange,
                "&r=", reporters,
                "&p=", partners,
                "&rg=", tradedirection,
                "&cc=", commodcodes,
                "&fmt=", fmt,
                "&head=", colname)

  if (!is.null(token)) {
    url <- paste0(url, "&token=", token)
  }

  # Execute API call using function "ct_csv_data" or "ct_json_data" (depending
  # on param fmt).
  if (fmt == "csv") {
    apires <- tryCatch(ct_csv_data(url, colname), error = function(e) e)
  } else if (fmt == "json") {
    apires <- tryCatch(ct_json_data(url, colname), error = function(e) e)
  }

  return(apires)
}
