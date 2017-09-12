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
#'
#' @return A dataframe of countries and country codes, downloaded from UN
#'  Comtrade.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples \dontrun{
#' countrydf <- ct_countries_table()
#' head(countrydf)
#' }
ct_countries_table <- function(reporters = NULL, partners = NULL) {

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
  reporters <- httr::GET(reporters)

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
  partners <- httr::GET(partners)

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
