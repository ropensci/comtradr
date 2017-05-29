#' Extract commodity code look up table from UN Comtrade
#'
#' Returns a dataframe of commodities, commodity codes and parent codes,
#' downloaded from UN Comtrade. For use with the UN Comtrade API, full API
#' docs can be found at \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param type Trade data classification scheme to use, see "Details" for a
#'  list of the valid inputs.
#' @param baseurl The base url of the UN Comtrade website.
#' @param path The path url string that points to the correct directory.
#' @param ssl_verify_peer logical, to be passed to param 'ssl_verifypeer'
#'  within the call to \code{httr::GET()}. Default is TRUE. Setting this to FALSE
#'  must be done with care, this should only be done if you trust the API site
#'  (https://comtrade.un.org/), and if you fully understand the security
#'  risks/implications of this decision.
#' @details The default for param \code{type} is \code{HS}. Below is a list of
#'  all valid inputs with a very brief description for each, for more
#'  information on each of these types, see
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
#'  The default for param \code{baseurl} is \code{"https://comtrade.un.org/"},
#'  and should only be changed if the Comtrade website url changes.
#'
#'  The default for param \code{path} is \code{"data/cache/"}, and should only
#'  be changed if the Comtrade website changes or if Comtrade changes the url
#'  path that points to the JSON commodity tables.
#' @return A dataframe of commodities, commodity codes and parent codes,
#'  downloaded from UN Comtrade.
#' @importFrom dplyr "%>%"
#' @export
#'
#' @examples \dontrun{
#' ct_commodities_table("HS")
#' ct_commodities_table("SITCrev2")
#' }
ct_commodities_table <- function(type = c("HS", "HS1992", "HS1996",
                                          "HS2002", "HS2007", "HS2012",
                                          "SITC", "SITCrev1", "SITCrev2",
                                          "SITCrev3", "SITCrev4", "BEC",
                                          "EB02"),
                                 baseurl = "https://comtrade.un.org/",
                                 path = "data/cache/",
                                 ssl_verify_peer = TRUE) {

  type <- match.arg(type)

  if (type == "HS") {
    url <- paste0(baseurl, path, "classificationHS.json")
  } else if (type == "HS1992") {
    url <- paste0(baseurl, path, "classificationH0.json")
  } else if (type == "HS1996") {
    url <- paste0(baseurl, path, "classificationH1.json")
  } else if (type == "HS2002") {
    url <- paste0(baseurl, path, "classificationH2.json")
  } else if (type == "HS2007") {
    url <- paste0(baseurl, path, "classificationH3.json")
  } else if (type == "HS2012") {
    url <- paste0(baseurl, path, "classificationH4.json")
  } else if (type == "SITC") {
    url <- paste0(baseurl, path, "classificationST.json")
  } else if (type == "SITCrev1") {
    url <- paste0(baseurl, path, "classificationS1.json")
  } else if (type == "SITCrev2") {
    url <- paste0(baseurl, path, "classificationS2.json")
  } else if (type == "SITCrev3") {
    url <- paste0(baseurl, path, "classificationS3.json")
  } else if (type == "SITCrev4") {
    url <- paste0(baseurl, path, "classificationS4.json")
  } else if (type == "BEC") {
    url <- paste0(baseurl, path, "classificationBEC.json")
  } else if (type == "EB02") {
    url <- paste0(baseurl, path, "classificationEB02.json")
  }

  rawdata <- tryCatch(
    httr::GET(url,
              config = httr::config(ssl_verifypeer = ssl_verify_peer)),
    error = function(e) e
  )

  if (methods::is(rawdata, "error")) {
    if (grepl("peer certificate cannot be authenticated", rawdata$message,
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

  if (httr::http_type(rawdata) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  rawdata <- rawdata %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)

  output <- rawdata$results %>%
    `colnames<-`(c("code", "commodity", "parent"))

  return(output)
}
