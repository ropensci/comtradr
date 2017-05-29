#' Extract country code look up table from UN Comtrade
#'
#' Returns a dataframe of countries and country codes,
#' downloaded from UN Comtrade. For use with the UN Comtrade API, full API
#' docs can be found at \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param reporters Default value NULL, otherwise this should be a url as a
#'  char string that points to the reporter areas JSON dataset on the Comtrade
#'  website. Only necessary if the Comtrade site changes from
#'  \url{https://comtrade.un.org/data/cache/reporterAreas.json}
#' @param partners Default value NULL, otherwise this should be a url as a
#'  char string that points to the reporter areas JSON dataset on the Comtrade
#'  website. Only necessary if the Comtrade site changes from
#'  \url{https://comtrade.un.org/data/cache/partnerAreas.json}
#' @param ssl_verify_peer logical, to be passed to param 'ssl_verifypeer'
#'  within the call to \code{httr::GET()}. Default is TRUE. Setting this to FALSE
#'  must be done with care, this should only be done if you trust the API site
#'  (https://comtrade.un.org/), and if you fully understand the security
#'  risks/implications of this decision.
#'
#' @return A dataframe of countries and country codes, downloaded from UN
#'  Comtrade.
#' @importFrom dplyr "%>%"
#' @export
#'
#' @examples \dontrun{
#' countrydf <- ct_countries_table()
#' }
ct_countries_table <- function(reporters = NULL, partners = NULL,
                               ssl_verify_peer = TRUE) {

  if (!is.null(reporters)) {
    reporters <- reporters
  } else {
    reporters <- "https://comtrade.un.org/data/cache/reporterAreas.json"
  }

  if (!is.null(partners)) {
    partners <- partners
  } else {
    partners <- "https://comtrade.un.org/data/cache/partnerAreas.json"
  }

  # pull reporters dataset from Comtrade
  reporters <- tryCatch(
    httr::GET(reporters,
              config = httr::config(ssl_verifypeer = ssl_verify_peer)),
    error = function(e) e
  )

  if (methods::is(reporters, "error")) {
    if (grepl("peer certificate cannot be authenticated", reporters$message,
              ignore.case = TRUE)) {
      msg <- paste0("Returned NULL. The SSL certificate of the API site can't ",
                    "be authenticated. You can try setting param ",
                    "'ssl_verify_peer' to FALSE, however this should only be ",
                    "done if you trust the API site ",
                    "(https://comtrade.un.org/), and if you fully understand",
                    " the security risks/implications of this decision.")
      warning(msg, call. = FALSE)
      return(NULL)
    }
  }

  if (httr::http_type(reporters) != "application/json") {
    stop("API did not return json for reporters", call. = FALSE)
  }

  reporters <- reporters %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  reporters <- reporters$results %>%
    `colnames<-`(c("code", "country name"))
  reporters$type <- "reporter"

  # Pull partners dataset from Comtrade
  partners <- tryCatch(
    httr::GET(partners,
              config = httr::config(ssl_verifypeer = ssl_verify_peer)),
    error = function(e) e
  )

  if (methods::is(partners, "error")) {
    if (grepl("peer certificate cannot be authenticated", partners$message,
              ignore.case = TRUE)) {
      msg <- paste0("Returned NULL. The SSL certificate of the API site can't ",
                    "be authenticated. You can try setting param ",
                    "'ssl_verify_peer' to FALSE, however this should only be ",
                    "done if you trust the API site ",
                    "(https://comtrade.un.org/), and if you fully understand",
                    " the security risks/implications of this decision.")
      warning(msg, call. = FALSE)
      return(NULL)
    }
  }

  if (httr::http_type(partners) != "application/json") {
    stop("API did not return json for partners", call. = FALSE)
  }

  partners <- partners %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  partners <- partners$results %>%
    `colnames<-`(c("code", "country name"))
  partners$type <- "partner"

  # rbind and return
  return(rbind(reporters, partners))
}
